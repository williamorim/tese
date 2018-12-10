# Libraries ---------------------------------------------------------------

library(tidyverse)
library(caret)
library(recipes)
library(lime)
library(patchwork)

# Dados -------------------------------------------------------------------

df_model <- read_rds("data/datasus-sim/model_mort_diaria_salvo.rds")

source("scripts/salvo-2017/salvo-utils.R")

# Formula -----------------------------------------------------------------

cria_formula <- function(mort) {
  
  padrao <- "hm + tp + trend + month + dayofweek "
  
  padrao %>% 
    str_c(mort, ., sep = " ~ ") %>% 
    as.formula()
  
}

cria_receita <- function(df_model, formula) {
  df_model %>%
    recipe(formula = formula) %>%
    step_naomit(all_predictors(), all_outcomes()) %>% 
    step_dummy(month, dayofweek, one_hot = TRUE)
}

vars <- c("n_mortes_geral", "n_mortes_idosos", "n_mortes_criancas")
formulas <- map(vars, cria_formula)
receitas <- map(formulas, cria_receita, df_model = df_model)

# Model -------------------------------------------------------------------

# Geral

train_control <- trainControl(method = "cv", number = 5)

tuning_grid <- expand.grid(
  gamma = 0,
  min_child_weight = 1,
  nrounds = 250,
  max_depth = 5,
  eta = 0.4,
  colsample_bytree = 1,
  subsample = 1
)

set.seed(5893524)

model <- train(
  x = receitas[[1]],
  data = na.omit(df_model),
  method = "xgbTree",
  trControl = train_control
  #tuneGrid = tuning_grid
  #importance = 'impurity'
)

model
model$finalModel
varImp(model)
# RMSE: 35.58922
# MAE: 28.3302
# % var: 0.6467966%  
# share_gas imp: 5ª

pred_obs_plot(
  obs = na.omit(df_model)$n_mortes_geral,
  pred = predict(model, newdata = na.omit(df_model))
)

# Idosos

train_control <- trainControl(method = "cv", number = 5)

tuning_grid <- expand.grid(
  splitrule = "variance",
  mtry = 13,
  min.node.size = 5
)

set.seed(5893524)

model <- train(
  x = receitas[[2]],
  data = na.omit(df_model),
  method = "ranger",
  trControl = train_control,
  tuneGrid = tuning_grid,
  importance = 'impurity'
)

model
model$finalModel
varImp(model)
# RMSE: 27.69438
# MAE: 21.87697
# % var: 0.6743066%  
# share_gas imp: 5ª

pred_obs_plot(
  obs = na.omit(df_model)$n_mortes_idosos,
  pred = predict(model, newdata = na.omit(df_model))
)
# Crianças

train_control <- trainControl(method = "cv", number = 5)

tuning_grid <- expand.grid(
  splitrule = "variance",
  mtry = 2,
  min.node.size = 5
)


set.seed(5893524)

model <- train(
  x = receitas[[3]],
  data = na.omit(df_model),
  method = "ranger",
  trControl = train_control,
  tuneGrid = tuning_grid,
  importance = 'impurity'
)

model
model$finalModel
varImp(model)
# RMSE: 5.045784
# MAE: 4.061302
# % var: 0.02881601%  
# share_gas imp: 4ª

pred_obs_plot(
  obs = na.omit(df_model)$n_mortes_criancas,
  pred = predict(model, newdata = na.omit(df_model))
)

