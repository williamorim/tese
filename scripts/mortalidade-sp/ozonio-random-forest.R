# Libraries ---------------------------------------------------------------

library(tidyverse)
library(caret)
library(recipes)
library(iml)
library(patchwork)

# Dados -------------------------------------------------------------------

df_model <- read_rds("data/datasus-sim/model_mort_diaria_salvo.rds")

# source("scripts/salvo-2017/salvo-utils.R")

# Formula -----------------------------------------------------------------

cria_formula <- function(mort) {
  
  padrao <- "o3_mass_conc + hm + tp + trend + dayofweek + dv_workday + month"
  
  padrao %>% 
    str_c(mort, ., sep = " ~ ")
  
}

cria_receita <- function(df_model, formula) {
  df_model %>%
    recipe(formula = formula) %>%
    step_naomit(all_predictors(), all_outcomes()) %>% 
    step_dummy(dayofweek, month, one_hot = TRUE)
}

vars <- c("n_mortes_idosos", "n_mortes_criancas")
formulas <- map(vars, cria_formula)
receitas <- map(formulas, cria_receita, df_model = df_model)

# Model -------------------------------------------------------------------

# Idosos

train_control <- trainControl(method = "cv", number = 5)

tuning_grid <- expand.grid(
  splitrule = "variance",
  mtry = 20,
  min.node.size = 3
)

set.seed(5893524)

model <- train(
  x = receitas[[1]],
  data = na.omit(df_model),
  method = "ranger",
  trControl = train_control,
  tuneGrid = tuning_grid,
  importance = 'impurity'
)

model
varImp(model)
# RMSE: 15.26366
# MAE: 21.7684
# % var: 0.4328288
# o3 imp: 6ª

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
# RMSE: 3.23273
# MAE: 2.584559
# % var: 0.007760021%  
# o3 imp: 4ª

pred_obs_plot(
  obs = na.omit(df_model)$n_mortes_criancas,
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
  splitrule = "variance",
  mtry = 20,
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
pdp <- FeatureEffect$new(predictor, feature = "o3_mass_conc", method = "pdp", 
                         grid.size = 20)
p_pdp <- pdp$plot() + 
  theme_bw() +
  labs(x = expression(paste(O[3], " (", mu, "g/", m^3, ")"))) +
  scale_y_continuous(name = "Mortalidade diária estimada") +
  ggtitle("PDP")

# ALE
ale <- FeatureEffect$new(predictor, feature = "o3_mass_conc", grid.size = 15)
p_ale <- ale$plot() + 
  theme_bw() +
  labs(x = expression(paste(O[3], " (", mu, "g/", m^3, ")"))) +
  scale_y_continuous(name = "Diferença em relação à predição média") +
  ggtitle("ALE")

p_pdp + p_ale
ggsave(
  filename = "text/figuras/cap-mort-ozonio-rf-graficos-iml.pdf", 
  width = 7, height = 5
)
