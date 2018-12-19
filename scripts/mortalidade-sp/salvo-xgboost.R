# Libraries ---------------------------------------------------------------

library(tidyverse)
library(caret)
library(recipes)

# Dados -------------------------------------------------------------------

df_model <- read_rds("data/datasus-sim/model_mort_diaria_salvo.rds")

#source("scripts/salvo-2017/salvo-utils.R")

# Formula -----------------------------------------------------------------

rec <- df_model %>%
  recipe(formula = tp ~ hm ) %>%
  step_naomit(all_predictors(), all_outcomes())
  #step_dummy(dayofweek, month)

# Model -------------------------------------------------------------------

# Idosos

train_control <- trainControl(method = "cv", number = 1)

tuning_grid <- expand.grid(
  gamma = 0,
  min_child_weight = 1,
  nrounds = 75,
  max_depth = 3,
  eta = 0.3,
  colsample_bytree = 0.9,
  subsample = 1
)

#set.seed(5893524)

#debugonce(caret:::train_rec)


model <- train(
  x = rec,
  data = df_model,
  method = "xgbTree",
  trControl = train_control,
  #tuneGrid = tuning_grid,
  importance = 'impurity'
)

model
model$finalModel
varImp(model)
# RMSE: 28.54686
# MAE: 22.65775
# % var: 0.6538584%  
# share_gas imp: 5ª

pred_obs_plot(
  obs = na.omit(df_model)$n_mortes_idosos,
  pred = predict(model, newdata = na.omit(df_model))
)


# Interpretação ------------------------------------------------------------

df_train <- receitas[[1]] %>% 
  prep(df_model) %>% 
  bake(df_model)

X <- df_train %>% 
  select(-n_mortes_idosos) %>% 
  as.matrix()

train_control <- trainControl(method = "cv", number = 5)

tuning_grid <- expand.grid(
  gamma = 0.01,
  min_child_weight = 1,
  nrounds = 75,
  max_depth = 3,
  eta = 0.3,
  colsample_bytree = 0.9,
  subsample = 1
)

set.seed(5893524)

model <- train(
  x = X,
  y = df_train$n_mortes_idosos,
  method = "xgbTree",
  trControl = train_control,
  tuneGrid = tuning_grid
)
predictor <- Predictor$new(model, data = df_train, y = df_train$n_mortes_idosos)

# PDP
pdp <- FeatureEffect$new(predictor, feature = "share_gas", method = "pdp", grid.size = 20)
p_pdp <- pdp$plot() + 
  theme_bw() +
  labs(x = "Proporção estimada de carros a gasolina") +
  scale_y_continuous(name = expression(paste(O[3], " estimado (", mu, "g/", m^3, ")"))) +
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
  filename = "text/figuras/cap-comb-xgboost-graficos-iml.pdf", 
  width = 7, height = 5
)
