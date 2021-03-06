# Ozônio - Salvo, 2017
# Random forest

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
  splitrule = "variance",
  mtry = 60,
  min.node.size = 3
)

# Caret

set.seed(5893524)

model <- train(
  x = rec,
  data = na.omit(df_model),
  method = "ranger",
  trControl = train_control,
  tuneGrid = tuning_grid,
  importance = 'impurity'
)

model
# RMSE: 14.11
# MAE: 10.20
# % var: 85.72%  
# share_gas imp: 6ª

model$finalModel
varImp(model)

pred_obs_plot(
  obs = na.omit(df_model)$o3_mass_conc,
  pred = predict(model, newdata = na.omit(df_model))
)
ggsave(
  filename = "text/figuras/cap-comb-forest-pred-obs-plot.pdf", 
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
  splitrule = "variance",
  mtry = 60,
  min.node.size = 3
)

model <- train(
  x = X,
  y = df_train$o3_mass_conc,
  method = "ranger",
  trControl = train_control,
  tuneGrid = tuning_grid,
  importance = 'impurity'
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
ale <- FeatureEffect$new(predictor, feature = "share_gas", grid.size = 20)
p_ale <- ale$plot() + 
  theme_bw() +
  labs(x = "Proporção estimada de carros a gasolina") +
  scale_y_continuous(name = "Diferença em relação à predição média") +
  ggtitle("ALE")

p_pdp + p_ale
ggsave(
  filename = "text/figuras/cap-comb-rf-graficos-iml.pdf", 
  width = 7, height = 5
)


# ALE (inglês)
ale <- FeatureEffect$new(predictor, feature = "share_gas", grid.size = 20)
p_ale <- ale$plot() + 
  theme_bw() +
  labs(x = "shareE25") +
  scale_y_continuous(name = "ALE") +
  ggtitle("Random forest")

p_ale_r <- p_ale

p_ale_r + p_ale_x
ggsave(
  filename = "text/figuras/cap-comb-rf-xgboost-p-ale.pdf",
  width = 7, height = 5
)

# Cenários com baixo e alto share -----------------------------------------

prep <- prep(rec, na.omit(df_model))
# Baixo
df_share_baixo <- df_model %>% 
  mutate(share_gas = 0.2) %>% 
  na.omit

df_share_alto <- df_model %>% 
  mutate(share_gas = 0.7) %>% 
  na.omit

df_model %>% 
  na.omit %>% 
  mutate(
    pred_baixa = predict(model, df_share_baixo),
    pred_alta = predict(model, df_share_alto),
    pred = predict(model, na.omit(df_model))
  ) %>%
  group_by(stationname) %>% 
  gather(var, o3, pred, pred_baixa, pred_alta) %>% 
  ggplot(aes(x = date, y = o3, color = var)) +
  geom_smooth(se = FALSE) +
  facet_wrap(~stationname) +
  labs(color = "Cenário", x = "Ano", y = "Ozônio predito") +
  scale_color_discrete(labels = c(
    "Proporção observada", "Proporção Alta", "Proporção baixa"
  )) +
  theme(legend.position = "bottom")
ggsave(
  filename = "text/figuras/cap-comb-random-forest-cenarios.pdf", 
  width = 9, 
  height = 6
)