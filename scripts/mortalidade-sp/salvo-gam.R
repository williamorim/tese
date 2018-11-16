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
  #temperatura = c("tp", "tp_var", "tp_min", "tp_max"),
  temperatura = c("tp", "tp_var"),
  #sazonalidade = c("month", "season")
  sazonalidade = "month"
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
  
  best <- ajuste$bestTune
  ajuste$results <- filter(
    ajuste$results, 
    select == best$select, 
    method == best$method
  )
  
  
  imp <-  ajuste %>% 
    varImp() %>% 
    .$importance %>% 
    rownames_to_column("var") %>%
    mutate(n = row_number(desc(Overall))) %>% 
    filter(var == preditor) %>% 
    .$n
  
  valor_p <- ajuste %>% 
    .$finalModel %>% 
    broom::tidy() %>% 
    filter(str_detect(term, preditor)) %>% 
    magrittr::extract(c("p.value")) %>% 
    as.list()
  
  res = tibble(
    RMSE = ajuste$results$RMSE,
    R2 = ajuste$results$Rsquared,
    MAE = ajuste$results$MAE,
    varImp = imp,
    valor_p = valor_p
  )
  
}

resultados <- map_dfr(
  ajustes,
  extrai_resultados,
  preditor = "share_gas"
) %>% 
  bind_cols(formulas, .)

# Geral

resultados %>% 
  filter(mortalidade == "n_mortes_geral") %>% 
  arrange(RMSE) %>% 
  View

# Melhor resultado:
# temp média
# month
# RMSE: 37.68864
# R2: 0.5965807
# varImp: 11
# valor-p: < 000.1

gam_plot(
  ajustes[[1]]$finalModel, 
  ajustes[[1]]$finalModel$smooth[[1]],
  xlab = ajustes[[1]]$finalModel$smooth[[1]]$term
)

gam_plot(
  ajustes[[1]]$finalModel, 
  ajustes[[1]]$finalModel$smooth[[4]],
  xlab = ajustes[[1]]$finalModel$smooth[[4]]$term
)


# Idosos

resultados %>% 
  filter(mortalidade == "n_mortes_idosos") %>% 
  arrange(RMSE) %>% 
  View

# Melhor resultado:
# temp média
# month
# RMSE: 29.55081
# R2: 0.6292175
# varImp: 10
# valor-p: < 000.1

gam_plot(
  ajustes[[2]]$finalModel, 
  ajustes[[2]]$finalModel$smooth[[1]],
  xlab = ajustes[[2]]$finalModel$smooth[[1]]$term
)

gam_plot(
  ajustes[[2]]$finalModel, 
  ajustes[[2]]$finalModel$smooth[[4]],
  xlab = ajustes[[2]]$finalModel$smooth[[4]]$term
)

# Crianças

resultados %>% 
  filter(mortalidade == "n_mortes_criancas") %>% 
  arrange(RMSE) %>% 
  View

# Melhor resultado:
# temp média
# month
# RMSE: 5.042236
# R2: 0.03484929
# varImp: 7
# valor-p: < 0.26

gam_plot(
  ajustes[[3]]$finalModel, 
  ajustes[[3]]$finalModel$smooth[[1]],
  xlab = ajustes[[3]]$finalModel$smooth[[1]]$term
)

gam_plot(
  ajustes[[3]]$finalModel, 
  ajustes[[3]]$finalModel$smooth[[4]],
  xlab = ajustes[[3]]$finalModel$smooth[[4]]$term
)
