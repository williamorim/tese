# Libraries ---------------------------------------------------------------

library(tidyverse)
library(caret)
library(recipes)

# Dados -------------------------------------------------------------------

df_model <- read_rds("data/datasus-sim/model_mort_diaria_salvo.rds")
df_share_bs <- read_rds("data/artaxo-salvo-geiger/dados_gas_200_bs.rds")

source("scripts/salvo-2017/salvo-utils.R")

# Formula -----------------------------------------------------------------

formulas <- df_model %>%
  select(
    -n_mortes_geral, -n_mortes_idosos, -n_mortes_criancas,
    -date, -dayofweek, -week, -contains("_reg"), -contains("_vac"),
    -pp, -dv_pp_20_150,
    -dv_sun_reg,
    -year, -month, -day, -dv_yearendvacation,
    -share_gas
  ) %>%
  names() %>%
  str_c(collapse = " + ") %>%
  str_c(
    c("n_mortes_geral ~ ", "n_mortes_idosos ~ ", "n_mortes_criancas ~ "), .
  ) %>%
  map(as.formula)

# Model -------------------------------------------------------------------

train_control <- trainControl(method="cv", number = 5)

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
# RMSE: 47.47
# MAE: 37.89
# % var: 36.16%
# share_gas imp: 4

pred_obs_plot(
  obs = na.omit(df_model)$n_mortes_geral,
  pred = predict(model, newdata = na.omit(df_model))
)

gam_plot(
  model$finalModel, 
  model$finalModel$smooth[[1]],
  xlab = "Concentração de ozônio"
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
