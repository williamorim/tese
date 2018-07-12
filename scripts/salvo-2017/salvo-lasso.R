# Ozônio - Salvo, 2017
# LASSO

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(caret)
library(recipes)

# Dados -------------------------------------------------------------------

df_model <- read_rds("data/artaxo-salvo-geiger/data-asg-model.rds")
df_share_bs <- read_rds("data/artaxo-salvo-geiger/dados_gas_200_bs.rds")

# Formula -----------------------------------------------------------------

formula <- df_model %>%
  select(
    -siteid,
    -date, -o3_mass_conc, -dayofweek,
    -starts_with("congestion"),
    -dv_kmregion_am_18_max, -dv_kmcity_am_80_max,
    -pp, -dv_pp_20_150,
    -dv_sun_reg,
    -year, -month, -day, -dv_weekday_regular, -dv_yearendvacation, -dv_o3
  ) %>%
  names() %>%
  str_c(collapse = " + ") %>%
  str_c("o3_mass_conc ~ ", .) %>%
  as.formula()

rec <- 
  df_model %>%
  na.omit() %>% 
  recipe(formula = formula) %>%
  step_naomit(all_predictors(), all_outcomes()) %>% 
  step_dummy(stationname, week) %>% 
  step_interact(
    terms = ~ matches("^stationname"):trend +
      matches("^stationname"):dv_beltway_open
  ) %>% 
  step_center(all_predictors(), -all_nominal()) %>% 
  step_scale(all_predictors(), -all_nominal())

# 5-fold cross-validation
train_control <- trainControl(method="cv", number = 5)
  
# Model -------------------------------------------------------------------

# LASSO
set.seed(5893524)

tuning_grid <- expand.grid(
  alpha = 1,
  lambda = seq(0, 1, 0.1)
)

model <- train(
  x = rec,
  data = na.omit(df_model), 
  method = "glmnet",
  tuneGrid = tuning_grid,
  trControl = train_control
)

model
# Modelo sem penalização escolhido

# RIDGE

set.seed(5893524)

tuning_grid <- expand.grid(
  alpha = 0,
  lambda = seq(0, 10, 1)
)

model <- train(
  x = rec,
  data = na.omit(df_model), 
  method = "glmnet",
  tuneGrid = tuning_grid,
  trControl = train_control
)

model

# Bootstrapping -----------------------------------------------------------

source("scripts/salvo-2017/salvo-bootstrapping.R")

fit_func <- function(data, i, rec, lambda, vars) {
  
  model <- train(
    x = rec,
    data = na.omit(data), 
    method = "glmnet",
    tuneGrid = data.frame(alpha = 1, lambda = lambda),
    trControl = trainControl(method = "none")
  )
  
  coef(model$finalModel, s = lambda)[vars, ]
  
}

set.seed(5893524)

coefs <- 
  map_dbl(
    1:200,
    bootstrapping,
    df = df_model,
    df_share_bs = df_share_bs,
    fit_func = fit_func,
    rec = rec,
    lambda = lambda,
    vars = "share_gas"
  )

sd(coefs)
