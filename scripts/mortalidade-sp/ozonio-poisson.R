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
    -date, -dayofweek,
    -pp, -dv_pp_20_150,
    -dv_sun_reg,
    -year, -month, -day, -dv_weekday_regular, -dv_yearendvacation,
    -share_gas,
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
  method = "glm",
  family = poisson(link = "log"),
  trControl = train_control
)

model
summary(model)
varImp(model)
# RMSE: 40.46
# MAE: 32.62
# % var: 53.59%
# share_gas imp: > 20
# aumento de 10 no ozônio -> aumento de 0.4% na taxa de mortalidade

pred_obs_plot(
  obs = na.omit(df_model)$n_mortes_geral,
  pred = predict(model, newdata = na.omit(df_model))
)

plot(model$finalModel)

# Idosos

set.seed(5893524)

model <- train(
  form = formulas[[2]],
  data = na.omit(df_model),
  method = "glm",
  family = poisson(link = "sqrt"),
  trControl = train_control
)

model
summary(model)
varImp(model)
# RMSE: 30.74
# MAE: 24.13
# % var: 61.15% 
# share_gas imp: > 20

pred_obs_plot(
  obs = na.omit(df_model)$n_mortes_idosos,
  pred = predict(model, newdata = na.omit(df_model))
)

plot(model$finalModel)

# Crianças

set.seed(5893524)

model <- train(
  form = formulas[[3]],
  data = na.omit(df_model),
  method = "glm",
  family = poisson(link = "sqrt"),
  trControl = train_control
)

model
summary(model)
varImp(model)
# RMSE: 5.19
# MAE: 4.16
# % var: 2.2%
# share_gas imp: 8

pred_obs_plot(
  obs = na.omit(df_model)$n_mortes_criancas,
  pred = predict(model, newdata = na.omit(df_model))
)

plot(model$finalModel)
