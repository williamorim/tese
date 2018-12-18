# Ozônio - Salvo, 2017
# GAM

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(caret)
library(recipes)
library(lime)
library(patchwork)
library(iml)

source("scripts/salvo-2017/salvo-utils.R")

# Dados -------------------------------------------------------------------

df_model <- read_rds("data/artaxo-salvo-geiger/data-asg-model.rds")

# Formula -----------------------------------------------------------------

formula <- df_model %>%
  select(
    -siteid,
    -date, -o3_mass_conc, -dayofweek,
    -starts_with("congestion"),
    -dv_kmregion_am_18_max, -dv_kmcity_am_80_max,
    -pp, -dv_pp_20_150,
    -dv_sun_reg,
    -year, -month, -day, -dv_weekday_regular, -dv_yearendvacation
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
  step_dummy(stationname, week, one_hot = TRUE) 

# Model -------------------------------------------------------------------

train_control <- trainControl(method = "cv", number = 5)

tuning_grid <- expand.grid(
  gamma = 0,
  min_child_weight = 1,
  nrounds = 300,
  max_depth = 5,
  eta = 0.4,
  colsample_bytree = 0.7,
  subsample = 1
)

# Caret

set.seed(5893524)

model <- train(
  x = rec,
  data = na.omit(df_model),
  method = "xgbTree",
  trControl = train_control,
  tuneGrid = tuning_grid,
  importance = 'impurity'
)

model
varImp(model)

# RMSE: 12.31828
# MAE: 8.802116
# % var: 0.8843525  
# share_gas imp: 6ª

pred_obs_plot(
  obs = na.omit(df_model)$o3_mass_conc,
  pred = predict(model, newdata = na.omit(df_model))
)
ggsave(
  filename = "text/figuras/cap-comb-xgboost-pred-obs-plot.pdf", 
  width = 6, 
  height = 4
)

# Interpretação ------------------------------------------------------------

df_train <- rec %>% 
  prep(df_model) %>% 
  bake(df_model)

X <- df_train %>% 
  select(-o3_mass_conc) %>% 
  as.matrix()

train_control <- trainControl(method = "cv", number = 5)

tuning_grid <- expand.grid(
  gamma = 0,
  min_child_weight = 1,
  nrounds = 300,
  max_depth = 5,
  eta = 0.4,
  colsample_bytree = 0.7,
  subsample = 1
)

set.seed(5893524)

model <- train(
  x = X,
  y = df_train$o3_mass_conc,
  method = "xgbTree",
  trControl = train_control,
  tuneGrid = tuning_grid
)
predictor <- Predictor$new(model, data = df_train, y = df_train$o3_mass_conc)

# PDP
pdp <- FeatureEffect$new(predictor, feature = "share_gas", method = "pdp", grid.size = 20)
p_pdp <- pdp$plot() + 
  theme_bw() +
  labs(x = "Proporção estimada de carros a gasolina") +
  scale_y_continuous(name = expression(paste(O[3], " estimado (", mu, "g/", m^3, ")"))) +
  ggtitle("PDP")

# ALE
ale <- FeatureEffect$new(predictor, feature = "share_gas", grid.size = 50)
p_ale <- ale$plot() + 
  theme_bw() +
  labs(x = "Proporção estimada de carros a gasolina") +
  scale_y_continuous(name = "Diferença em relação à predição média") +
  ggtitle("ALE")

p_pdp + p_ale
ggsave(
  filename = "text/figuras/cap-comb-xgboost-graficos-iml.pdf", 
  width = 7, height = 5
)

