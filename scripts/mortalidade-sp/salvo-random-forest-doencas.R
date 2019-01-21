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
    str_c(mort, ., sep = " ~ ")
  
}

cria_receita <- function(df_model, formula) {
  df_model %>%
    recipe(formula = formula) %>%
    step_naomit(all_predictors(), all_outcomes()) %>% 
    step_dummy(dayofweek, month, one_hot = TRUE)
}

vars <- c("n_mortes_idosos2", "n_mortes_criancas2")
formulas <- map(vars, cria_formula)
receitas <- map(formulas, cria_receita, df_model = df_model)

# Model -------------------------------------------------------------------

# Idosos

train_control <- trainControl(method = "cv", number = 5)

tuning_grid <- expand.grid(
  splitrule = "variance",
  mtry = 13,
  min.node.size = 5
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
# RMSE: 11.25617
# MAE: 9.012871
# % var: 0.4164112 
# share_gas imp: 4ª

pred_obs_plot(
  obs = na.omit(df_model)$n_mortes_idosos2,
  pred = predict(model, newdata = na.omit(df_model))
)

# Interpretação ------------------------------------------------------------

df_train <- receitas[[1]] %>% 
  prep(df_model) %>% 
  bake(df_model)

X <- df_train %>% 
  select(-n_mortes_idosos2) %>% 
  as.matrix()

train_control <- trainControl(method = "cv", number = 5)

tuning_grid <- expand.grid(
  splitrule = "variance",
  mtry = 13,
  min.node.size = 5
)

model <- train(
  x = X,
  y = df_train$n_mortes_idosos2,
  method = "ranger",
  trControl = train_control,
  tuneGrid = tuning_grid,
  importance = 'impurity'
)

predictor <- Predictor$new(model, data = df_train, y = df_train$n_mortes_idosos2)

# PDP
pdp <- FeatureEffect$new(predictor, feature = "share_gas", method = "pdp", 
                         grid.size = 20)
p_pdp <- pdp$plot() + 
  theme_bw() +
  labs(x = "Proporção estimada de carros a gasolina") +
  scale_y_continuous(name = "Mortalidade diária estimada") +
  ggtitle("PDP")

# ALE
ale <- FeatureEffect$new(predictor, feature = "share_gas", grid.size = 20)
p_ale <- ale$plot() + 
  theme_bw() +
  labs(x = "Proporção estimada de carros a gasolina") +
  scale_y_continuous(name = "Diferença em relação à predição média") +
  ggtitle("ALE")

p_pdp + p_ale
ggsave(
  filename = "text/figuras/cap-mort-rf-doencas-graficos-iml.pdf", 
  width = 7, height = 5
)



