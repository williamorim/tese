# Libraries ---------------------------------------------------------------

library(tidyverse)
library(caret)
library(recipes)
library(lime)
library(patchwork)

# Dados -------------------------------------------------------------------

df_model <- read_rds("data/datasus-sim/model_mort_diaria_salvo.rds")

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

train_control <- trainControl(method = "cv", number = 5)

tuning_grid <- expand.grid(
  splitrule = "variance",
  mtry = c(60, 80, 100),
  min.node.size = c(1, 5, 10)
)

# Geral

set.seed(5893524)

model <- train(
  x = recs[[1]],
  data = na.omit(df_model),
  method = "ranger",
  trControl = train_control,
  #tuneGrid = tuning_grid,
  importance = 'impurity'
)

model
# RMSE: 14.11
# MAE: 10.20
# % var: 85.72%  
# share_gas imp: 6ª
