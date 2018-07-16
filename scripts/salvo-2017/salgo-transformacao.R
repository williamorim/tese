# Ozônio - Salvo, 2017
# Modelos com transformação na resposta

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(caret)
library(recipes)
library(patchwork)

# Dados -------------------------------------------------------------------

df_model <- read_rds("data/artaxo-salvo-geiger/data-asg-model.rds")

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

#LOG

rec <- 
  df_model %>%
  na.omit() %>% 
  recipe(formula = formula) %>%
  step_naomit(all_predictors(), all_outcomes()) %>% 
  step_dummy(stationname, week) %>% 
  step_interact(
    terms = ~ matches("^stationname"):trend +
      matches("^stationname"):dv_beltway_open
  ) %>% 
  step_log(all_outcomes())

# RMSE para escala log
summary_log <- function(data, lev = NULL, model = NULL) {
  
  metrics <- defaultSummary(data, lev, model)
  
  residuo <- exp(data$obs) - exp(data$pred)
  metrics["RMSE"] <- sqrt(mean((residuo)^2))
  metrics["MAE"] <- mean(abs(residuo))
  
  metrics
}

# 5-fold cross-validation
train_control <- trainControl(
  method = "cv", 
  number = 5,
  summaryFunction = summary_log 
)

set.seed(5893524)

model <- train(
  x = rec,
  data = na.omit(df_model), 
  method = "lm", 
  trControl = train_control
)

model
varImp(model)
# RMSE: 21.18
# MAE: 15.41
# % var: 71.31%  
# share_gas imp: 14ª

p1 <- pred_obs_plot(
  obs = log(na.omit(df_model)$o3_mass_conc),
  pred = predict(model, newdata = na.omit(df_model))
)

#BOX-COX

inv_boxcox <- function(x, lambda = 0.5099256) {
  (lambda*x + 1)^(1/lambda)
}

rec <- 
  df_model %>%
  na.omit() %>% 
  recipe(formula = formula) %>%
  step_naomit(all_predictors(), all_outcomes()) %>% 
  step_dummy(stationname, week) %>% 
  step_interact(
    terms = ~ matches("^stationname"):trend +
      matches("^stationname"):dv_beltway_open
  ) %>% 
  step_BoxCox(all_outcomes())

summary_box <- function(data, lev = NULL, model = NULL) {
  
  metrics <- defaultSummary(data, lev, model)
  
  residuo <- inv_boxcox(data$obs) - inv_boxcox(data$pred)
  metrics["RMSE"] <- sqrt(mean((residuo)^2))
  metrics["MAE"] <- mean(abs(residuo))
  
  metrics
}

# 5-fold cross-validation
train_control <- trainControl(
  method = "cv", 
  number = 5,
  summaryFunction = summary_box
)

set.seed(5893524)

model <- train(
  x = rec,
  data = na.omit(df_model), 
  method = "lm", 
  trControl = train_control
)

model
varImp(model)
summary(model)
# RMSE: 19.48
# MAE: 14.44
# % var: 74.02%  
# share_gas imp: 16º

df_test <- prep(rec, training = df_model) %>% bake(newdata = df_model)

p2 <- pred_obs_plot(
  obs = df_test$o3_mass_conc,
  pred = predict(model, newdata = na.omit(df_model))
)

p <- p1 + p2

ggsave(
  filename = "text/figuras/cap-comb-lin-reg-transfor-pred-obs-plot.pdf",
  width = 6,
  height = 4
)
 
# BOX-COX & Random Forest

rec <- 
  df_model %>%
  na.omit() %>% 
  recipe(formula = formula) %>%
  step_naomit(all_predictors(), all_outcomes()) %>% 
  step_dummy(stationname, week, one_hot = TRUE) %>% 
  step_interact(
    terms = ~ matches("^stationname"):trend +
      matches("^stationname"):dv_beltway_open
  ) %>% 
  step_BoxCox(all_outcomes())

summary_box <- function(data, lev = NULL, model = NULL) {
  
  metrics <- defaultSummary(data, lev, model)
  
  residuo <- inv_boxcox(data$obs) - inv_boxcox(data$pred)
  metrics["RMSE"] <- sqrt(mean((residuo)^2))
  metrics["MAE"] <- mean(abs(residuo))
  
  metrics
}

# 5-fold cross-validation
train_control <- trainControl(
  method = "cv", 
  number = 5,
  summaryFunction = summary_box
)

tuning_grid <- expand.grid(
  splitrule = "variance",
  mtry = c(47, 48, 49),
  min.node.size = 1
)

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
model$finalModel
varImp(model)

# RMSE: 14.07
# MAE: 10.12
# % var: 86.74%  
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
