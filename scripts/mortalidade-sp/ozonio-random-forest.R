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

vars <- c("n_mortes_geral", "n_mortes_idosos", "n_mortes_criancas")
formulas <- map(vars, cria_formula)
receitas <- map(formulas, cria_receita, df_model = df_model)

# Model -------------------------------------------------------------------

# Idosos

train_control <- trainControl(method = "cv", number = 5)

tuning_grid <- expand.grid(
  splitrule = "variance",
  mtry = 13,
  min.node.size = 4
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
# RMSE: 28.46537
# MAE: 21.87697
# % var: 0.6557702 
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
# RMSE: 5.054336
# MAE: 4.06617
# % var: 0.02515931%  
# o3 imp: 3ª

pred_obs_plot(
  obs = na.omit(df_model)$n_mortes_criancas,
  pred = predict(model, newdata = na.omit(df_model))
)

# Lime --------------------------------------------------------------------

make_explanation <- function(i, explainer, df, n_features = 5) {
  
  explanation <- explain(df_explain[i,], explainer, n_features = n_features) %>% 
    select(
      feature, 
      feature_value,
      feature_weight,
      prediction,
      model_r2
    ) %>% 
    mutate(feature_value = as.character(feature_value))
  
  if(i%%100 == 0) {
    print(paste("Another one bites the dust!", i))
  }
  
  explanation
  
}

explainer <- lime(
  na.omit(df_model), 
  model
)

df_explain <- na.omit(df_model)
m <- nrow(df_explain)

explanation <- map_dfr(
  1:m,
  make_explanation,
  explainer = explainer,
  df = df_explain
)

# saveRDS(explanation, file = "explanation.rds")
# explanation <- readRDS("explanation.rds")

# Explicação geral
explanation %>% 
  filter(feature == "o3_mass_conc") %>%
  mutate(feature_value = as.numeric(feature_value)) %>% 
  ggplot(aes(x = feature_value, y = feature_weight)) +
  geom_point() +
  labs(
    x = "Proporção de carros a gasolina",
    y = "Coeficiente no modelo simples"
  ) +
  theme_bw()

ggsave(
  filename = "text/figuras/cap-mort-share-rf-explanation.pdf",
  width = 6,
  height = 4
)


