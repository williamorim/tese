# Libraries ---------------------------------------------------------------

library(tidyverse)
library(caret)
library(recipes)
library(iml)

# Dados -------------------------------------------------------------------

df_model <- read_rds("data/datasus-sim/model_mort_diaria_salvo.rds")

df_model <- df_model %>% 
  mutate(
    n_mortes_idosos = as.numeric(n_mortes_idosos),
    share_gas = as.numeric(share_gas),
    month = paste("mes", month, sep = "_"),
    dayofweek = paste("dia", dayofweek, sep = "_")
  )

#source("scripts/salvo-2017/salvo-utils.R")

# Formula -----------------------------------------------------------------

rec <- df_model %>%
  recipe(
    n_mortes_idosos ~ share_gas + hm + tp + trend + dv_workday + month + dayofweek 
  ) %>%
  step_dummy(dayofweek, month, one_hot = TRUE)

# Model -------------------------------------------------------------------

# Idosos

df_train <- rec %>% 
  prep(df_model) %>% 
  bake(df_model)

X <- df_train %>% 
  select(-n_mortes_idosos) %>% 
  as.matrix()

train_control <- trainControl(method = "cv", number = 5)

tuning_grid <- expand.grid(
  gamma = 0.01,
  min_child_weight = 0.1,
  nrounds = 200,
  max_depth = 2,
  eta = 0.3,
  colsample_bytree = 0.8,
  subsample = 0.9
)

set.seed(5893524)

model <- train(
  x = X,
  y = df_train$n_mortes_idosos,
  method = "xgbTree",
  trControl = train_control,
  tuneGrid = tuning_grid
)

model
model$finalModel
varImp(model)
# RMSE: 28.12986
# MAE: 22.14141
# % var: 0.6745081  
# share_gas imp: 5ª


# Interpretação ------------------------------------------------------------

predictor <- Predictor$new(model, data = df_train, y = df_train$n_mortes_idosos)

# PDP
pdp <- FeatureEffect$new(predictor, feature = "share_gas", method = "pdp", grid.size = 15)
p_pdp <- pdp$plot() + 
  theme_bw() +
  labs(x = "Proporção estimada de carros a gasolina") +
  scale_y_continuous(name = expression(paste(O[3], " estimado (", mu, "g/", m^3, ")"))) +
  ggtitle("PDP")

# ALE
ale <- FeatureEffect$new(predictor, feature = "share_gas", grid.size = 15)
p_ale <- ale$plot() + 
  theme_bw() +
  labs(x = "Proporção estimada de carros a gasolina") +
  scale_y_continuous(name = "Diferença em relação à predição média") +
  ggtitle("ALE")

p_pdp + p_ale
ggsave(
  filename = "text/figuras/cap-mort-xgboost-graficos-iml.pdf", 
  width = 7, height = 5
)
