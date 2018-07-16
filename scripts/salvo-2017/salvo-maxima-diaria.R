# Ozônio - Salvo, 2017
# Modelos com máxima diária

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(caret)
library(recipes)
library(patchwork)

# Dados -------------------------------------------------------------------

df_model <- read_rds("data/artaxo-salvo-geiger/data-asg-model.rds")
df_salvo <- read_rds("data/artaxo-salvo-geiger/dados_originais.rds") %>% 
  select(o3_mass_conc, date, siteid)

df_model <- df_salvo %>%
  filter(
    siteid %in% c(1, 2, 3, 5, 6, 7, 15, 18, 22, 27, 29, 31),
    !lubridate::month(date) %in% 6:9
  ) %>%
  mutate(siteid = as.factor(siteid)) %>% 
  group_by(date, siteid) %>% 
  summarise(o3_mass_conc = max(o3_mass_conc, na.rm = TRUE)) %>% 
  mutate(o3_mass_conc = ifelse(is.finite(o3_mass_conc), o3_mass_conc, NA)) %>% 
  left_join(select(df_model, - o3_mass_conc), ., by = c("date", "siteid"))

rm(df_salvo)

source("scripts/salvo-2017/salvo-utils.R")

# Formula -----------------------------------------------------------------

formula <- df_model %>%
  select(
    -siteid,
    -date, -o3_mass_conc, -dayofweek,
    -starts_with("congestion"),
    -dv_kmregion_am_18_max, -dv_kmcity_am_80_max,
    -pp, -dv_pp_20_150,
    -dv_sun_reg,
    -year, -month, -day, -dv_weekday_regular, -dv_yearendvacation, -dv_o3
  ) %>%
  names() %>%
  str_c(collapse = " + ") %>%
  str_c("o3_mass_conc ~ ", .) %>%
  as.formula()

# Linear regression -------------------------------------------------------

set.seed(5893524)

# 5-fold cross-validation
train_control <- trainControl(method="cv", number = 5)

model <- train(
  form = formula, 
  data = na.omit(df_model), 
  method = "lm", 
  trControl = train_control
)

model
summary(model$finalModel)
varImp(model)

# RMSE: 28.00
# MAE: 20.58
# % var: 61.01%  
# share_gas imp: 11º

pred_obs_plot(
  obs = na.omit(df_model)$o3_mass_conc,
  pred = predict(model, newdata = na.omit(df_model))
)
# ggsave(
#   filename = "text/figuras/cap-comb-lin-reg-pred-obs-plot.pdf", 
#   width = 6, 
#   height = 4
# )


# Random Forest -----------------------------------------------------------

rec <- 
  df_model %>%
  na.omit() %>% 
  recipe(formula = formula) %>%
  step_naomit(all_predictors(), all_outcomes()) %>% 
  step_dummy(stationname, week, one_hot = TRUE) %>% 
  step_interact(
    terms = ~ matches("^stationname"):trend +
      matches("^stationname"):dv_beltway_open
  )

train_control <- trainControl(method = "cv", number = 5)

tuning_grid <- expand.grid(
  splitrule = "variance",
  mtry = c(40, 45, 48, 50),
  min.node.size = c(1, 5, 10)
)

set.seed(5893524)

model <- train(
  form = formula,
  data = na.omit(df_model),
  method = "ranger",
  trControl = train_control,
  tuneGrid = tuning_grid,
  importance = 'impurity'
)

model
model$finalModel
varImp(model)

# RMSE: 21.17
# MAE: 15.41
# % var: 70.92%  
# share_gas imp: 14º

df_test <- prep(rec, training = df_model) %>% bake(newdata = df_model)

pred_obs_plot(
  obs = df_test$o3_mass_conc,
  pred = predict(model, newdata = na.omit(df_model))
)
# ggsave(
#   filename = "text/figuras/cap-comb-lin-reg-pred-obs-plot.pdf", 
#   width = 6, 
#   height = 4
# )

