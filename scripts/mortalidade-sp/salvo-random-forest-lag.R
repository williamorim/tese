# Libraries ---------------------------------------------------------------

library(tidyverse)
library(caret)
library(recipes)
library(iml)
library(patchwork)

# Dados -------------------------------------------------------------------

df_model <- read_rds("data/datasus-sim/model_mort_diaria_salvo.rds")

source("scripts/salvo-2017/salvo-utils.R")

# Formula -----------------------------------------------------------------

cria_formula <- function(mort) {
  
  padrao <- "share_gas + hm + tp + trend + dayofweek + dv_workday + month"
  
  padrao %>% 
    str_c(mort, ., sep = " ~ ") %>% 
    as.formula()
  
}

cria_receita <- function(df_model, formula) {
  
  rec <- recipe(formula = formula, data = df_model) %>% 
    step_dummy(dayofweek, month, one_hot = TRUE)
  
}

receitas <- expand.grid(
  mortalidade = c("n_mortes_idosos"),
  lag = 1:21
) %>% 
  as.tibble() %>% 
  mutate(
    formula = map(mortalidade, cria_formula),
    receita = map(formula, cria_receita, df_model = df_model)
  )

# Model -------------------------------------------------------------------

# Function to train the models

train_model <- function(receita, lag, df_model, train_control, tuning_grid) {
  
  set.seed(5893524)
  
  df_model <- df_model %>% 
    mutate(share_gas = lag(share_gas, lag))
  
  train(
    receita,
    data = na.omit(df_model),
    method = "ranger",
    trControl = train_control, 
    tuneGrid = tuning_grid,
    importance = 'impurity'
  )
  
}

# Fits

train_control <- trainControl(method="cv", number = 5)

tuning_grid <- expand.grid(
  splitrule = "variance",
  mtry = 12,
  min.node.size = 3
)

ajustes <- map2(
  receitas$receita,
  receitas$lag,
  train_model,
  df_model = filter(df_model),
  train_control = train_control,
  tuning_grid = tuning_grid
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
  
  tibble(
    RMSE = ajuste$results$RMSE,
    R2 = ajuste$results$Rsquared,
    MAE = ajuste$results$MAE,
    varImp = imp
  )
  
}

resultados <- map_dfr(
  ajustes,
  extrai_resultados,
  preditor = "share_gas"
) %>% 
  bind_cols(receitas, .) %>% 
  select(-formula, -receita)

# Idosos

resultados %>% 
  filter(mortalidade == "n_mortes_idosos") %>% 
  arrange(RMSE) %>% 
  View


# Interpretação ------------------------------------------------------------

df_model_ <- df_model %>% 
  mutate(share_gas = lag(share_gas, 3)) %>% 
  na.omit()

df_train <- receitas$receita[[3]] %>%
  prep(df_model_) %>% 
  bake(na.omit(df_model_))

X <- df_train %>% 
  select(-n_mortes_idosos) %>% 
  as.matrix()

train_control <- trainControl(method="cv", number = 5)

tuning_grid <- expand.grid(
  splitrule = "variance",
  mtry = 12,
  min.node.size = 3
)

model <- train(
  x = X,
  y = df_train$n_mortes_idosos,
  method = "ranger",
  trControl = train_control,
  tuneGrid = tuning_grid,
  importance = 'impurity'
)

predictor <- Predictor$new(model, data = df_train, y = df_train$n_mortes_idosos)

# PDP
pdp <- FeatureEffect$new(predictor, feature = "share_gas", method = "pdp", 
                         grid.size = 10)
p_pdp <- pdp$plot() + 
  theme_bw() +
  labs(x = "Proporção estimada de carros a gasolina") +
  scale_y_continuous(name = "Mortalidade diária estimada") +
  ggtitle("PDP")

# ALE
ale <- FeatureEffect$new(predictor, feature = "share_gas", grid.size = 10)
p_ale <- ale$plot() + 
  theme_bw() +
  labs(x = "Proporção estimada de carros a gasolina") +
  scale_y_continuous(name = "Diferença em relação à predição média") +
  ggtitle("ALE")

p_pdp + p_ale
ggsave(
  filename = "text/figuras/cap-mort-rf-defasada-graficos-iml.pdf", 
  width = 7, height = 5
)
