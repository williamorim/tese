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

formulas <- df_model %>%
  select(
    -n_mortes_geral, -n_mortes_idosos, -n_mortes_criancas,
    -date, -dayofweek,
    -pp, -dv_pp_20_150,
    -dv_sun_reg,
    -year, -month, -day, -dv_weekday_regular, -dv_yearendvacation
  ) %>%
  names() %>%
  str_c(collapse = " + ") %>%
  str_c(
    c("n_mortes_geral ~ ", "n_mortes_idosos ~ ", "n_mortes_criancas ~ "), .
  ) %>%
  map(as.formula)

make_recs <- function(df_model, formula, lag) {
  df_model %>%
    recipe(formula = formula) %>%
    step_lag(share_gas, lag = lag) %>%
    step_dummy(week, one_hot = TRUE) %>% 
    step_rm(share_gas) %>% 
    step_meanimpute(all_numeric())
}

recs_geral <- map(1:30, make_recs, df_model = df_model, formula = formulas[[1]])
recs_idosos <- map(1:30, make_recs, df_model = df_model, formula = formulas[[2]])
recs_criancas <- map(1:30, make_recs, df_model = df_model, formula = formulas[[3]])

# Model -------------------------------------------------------------------

# Geral

run_model <- function(rec, df_model, train_control, tuning_grid) {
  set.seed(5893524)
  model <- train(
    x = rec,
    data = df_model,
    method = "ranger",
    trControl = train_control,
    tuneGrid = tuning_grid,
    importance = 'impurity'
  )
  model$results
}

train_control <- trainControl(method = "cv", number = 5)

tuning_grid <- expand.grid(
  splitrule = "variance",
  mtry = 61,
  min.node.size = 6
)

geral_results <- map_dfr(
  recs_geral, 
  run_model, 
  df_model = df_model,
  train_control = train_control,
  tuning_grid = tuning_grid
)

# Idosos

train_control <- trainControl(method = "cv", number = 5)

tuning_grid <- expand.grid(
  splitrule = "variance",
  mtry = 68,
  min.node.size = 5
)

set.seed(5893524)

model <- train(
  x = recs[[2]],
  data = na.omit(df_model),
  method = "ranger",
  trControl = train_control,
  tuneGrid = tuning_grid,
  importance = 'impurity'
)

model
model$finalModel
varImp(model)
# RMSE: 27.91
# MAE: 21.85
# % var: 69.47%  
# share_gas imp: 2ª

pred_obs_plot(
  obs = na.omit(df_model)$n_mortes_idosos,
  pred = predict(model, newdata = na.omit(df_model))
)
# Crianças

train_control <- trainControl(method = "cv", number = 5)

tuning_grid <- expand.grid(
  splitrule = "variance",
  mtry = 2,
  min.node.size = 3
)


set.seed(5893524)

model <- train(
  x = recs[[3]],
  data = na.omit(df_model),
  method = "ranger",
  trControl = train_control,
  tuneGrid = tuning_grid,
  importance = 'impurity'
)

model
model$finalModel
varImp(model)
# RMSE: 5.13
# MAE: 4.12
# % var: 1.7%  
# share_gas imp: 4ª

pred_obs_plot(
  obs = na.omit(df_model)$n_mortes_criancas,
  pred = predict(model, newdata = na.omit(df_model))
)