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

make_recs <- function(df_model, formula) {
  df_model %>%
    #na.omit() %>% 
    recipe(formula = formula) %>%
    step_naomit(all_predictors(), all_outcomes()) %>% 
    step_dummy(week, one_hot = TRUE)
}

recs <- map(formulas, make_recs, df_model = df_model)


# Model -------------------------------------------------------------------

# Geral

train_control <- trainControl(method = "cv", number = 5)

tuning_grid <- expand.grid(
  splitrule = "variance",
  mtry = 61,
  min.node.size = 6
)

set.seed(5893524)

model <- train(
  x = recs[[1]],
  data = na.omit(df_model),
  method = "ranger",
  trControl = train_control,
  tuneGrid = tuning_grid,
  importance = 'impurity'
)

model
model$finalModel
varImp(model)
# RMSE: 36.06
# MAE: 28.60
# % var: 64.57%  
# share_gas imp: 3ª

pred_obs_plot(
  obs = na.omit(df_model)$n_mortes_geral,
  pred = predict(model, newdata = na.omit(df_model))
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

# LIME --------------------------------------------------------------------

# Idosos

explainer <- lime(
  na.omit(df_model), 
  model
)

# 10% mais mortes

test_days <- df_model %>% 
  na.omit %>%
  top_n(n = 100, n_mortes_idosos)

explanation <- explain(
  test_days, 
  explainer,
  n_features = 10
)

explanation %>% 
  filter(feature == "share_gas") %>%
  mutate(feature_value = as.numeric(feature_value)) %>% 
  ggplot(aes(x = feature_value, y = feature_weight)) +
  geom_point() +
  labs(
    x = "Proporção de carros a gasolina",
    y = "Coeficiente no modelo simples"
  ) +
  #ggtitle("Maiores médias de ozônio") +
  theme_bw()

# 10% menos mortes

test_days <- 
  df_model %>% 
  na.omit %>%
  top_n(n = -100, n_mortes_idosos)

explanation <- explain(
  test_days, 
  explainer, 
  n_features = 10
)

explanation %>% 
  filter(feature == "share_gas") %>%
  mutate(feature_value = as.numeric(feature_value)) %>% 
  ggplot(aes(x = feature_value, y = feature_weight)) +
  geom_point() +
  labs(
    x = "Proporção de carros a gasolina",
    y = "Coeficiente no modelo simples"
  ) +
  ggtitle("Menores médias de ozônio") +
  theme_bw()

# p <- p2 + p1
# 
# ggsave(
#   plot = p,
#   filename = paste(
#     "scripts/salvo-2017/img/random-forest-explanations/explanation-",
#     station,
#     ".pdf"
#   ),
#   width = 6, 
#   height = 4
# )