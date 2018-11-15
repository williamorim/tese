# Libraries ---------------------------------------------------------------

library(tidyverse)
library(caret)
library(recipes)

# Dados -------------------------------------------------------------------

df_model <- read_rds("data/datasus-sim/model_mort_diaria_salvo.rds")
#df_share_bs <- read_rds("data/artaxo-salvo-geiger/dados_gas_200_bs.rds")

source("scripts/salvo-2017/salvo-utils.R")

# Formulas ----------------------------------------------------------------

cria_formula <- function(mort, temp, sazon) {
  
  padrao <- "share_gas + hm + trend + dayofweek + dv_workday"
  
  
  padrao %>% 
    str_c(temp, sep = " + ") %>% 
    str_c(sazon, sep = " + ") %>% 
    str_c(mort, ., sep = " ~ ")
  
}

formulas <- expand.grid(
  mortalidade = c("n_mortes_geral", "n_mortes_idosos", "n_mortes_criancas"),
  temperatura = c("tp", "tp_var", "tp_min", "tp_max"),
  sazonalidade = c("month", "season")
) %>% 
  as.tibble() %>% 
  mutate(formula = cria_formula(mortalidade, temperatura, sazonalidade))

# Model -------------------------------------------------------------------

# Function to train the models

train_model <- function(formula, df_model, train_control) {
  
  set.seed(5893524)
  
  train(
    form = as.formula(formula),
    data = na.omit(df_model),
    method = "gam",
    family = poisson(link = "log"),
    trControl = train_control
  )
  
}

# Fits

train_control <- trainControl(method="cv", number = 5)

ajustes <- map(
  formulas$formula,
  train_model,
  df_model = df_model,
  train_control = train_control
)

# Resultados

extrai_resultados <- function(ajuste, preditor) {
  
  imp <-  ajuste %>% 
    varImp() %>% 
    .$importance %>% 
    rownames_to_column("var") %>%
    mutate(n = row_number(desc(Overall))) %>% 
    filter(var == preditor) %>% 
    .$n
  
  estimativas <- ajuste %>% 
    .$finalModel %>% 
    broom::tidy() %>% 
    filter(term == preditor) %>% 
    magrittr::extract(c("estimate", "p.value")) %>% 
    as.list()
  
  tibble(
    RMSE = ajuste$results$RMSE,
    R2 = ajuste$results$Rsquared,
    MAE = ajuste$results$MAE,
    varImp = imp,
    coeficiente = estimativas$estimate,
    valor_p = estimativas$p.value
  ) %>% 
    mutate(
      variacao = round((exp(coeficiente*10) - 1)*100, 2)
    )
  
}

resultados <- map_dfr(
  ajustes,
  extrai_resultados,
  preditor = "share_gas"
) %>% 
  bind_cols(formulas, .)


















# Geral

set.seed(5893524)

model <- train(
  form = formulas[[1]],
  data = na.omit(df_model),
  method = "gam",
  family = poisson(link = "log"),
  trControl = train_control
)

model
summary(model)
varImp(model)
# RMSE: 43.45
# MAE: 34.56
# % var: 46.42%
# share_gas imp: 2

pred_obs_plot(
  obs = na.omit(df_model)$n_mortes_geral,
  pred = predict(model, newdata = na.omit(df_model))
)

gam_plot(
  model$finalModel, 
  model$finalModel$smooth[[1]],
  xlab = "Proporção estimada de carros a gasolina"
)

# Idosos

set.seed(5893524)

model <- train(
  form = formulas[[2]],
  data = na.omit(df_model),
  method = "gam",
  family = poisson(link = "sqrt"),
  trControl = train_control
)

model
summary(model)
varImp(model)
# RMSE: 33.50
# MAE: 26.33
# % var: 53.63%
# share_gas imp: > 20

pred_obs_plot(
  obs = na.omit(df_model)$n_mortes_idosos,
  pred = predict(model, newdata = na.omit(df_model))
)

gam_plot(
  model$finalModel, 
  model$finalModel$smooth[[1]],
  xlab = "Proporção estimada de carros a gasolina"
)
