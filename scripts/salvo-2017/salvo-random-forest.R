# Ozônio - Salvo, 2017
# GAM

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(caret)
library(recipes)

# Dados -------------------------------------------------------------------

df_model <- read_rds("data/artaxo-salvo-geiger/data-asg-model.rds")

# Formula -----------------------------------------------------------------

df_model <- 
  recipe(df_model) %>% 
  step_dummy(stationname) %>% 
  step_interact(
    terms = ~ matches("^stationname"):trend +
      matches("^stationname"):dv_beltway_open
  ) %>% 
  prep(training = df_model) %>% 
  bake(newdata = df_model)

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

# Model -------------------------------------------------------------------

train_control <- trainControl(method="cv", number = 5)

# Caret

model <- train(
  form = formula,
  data = na.omit(df_model),
  trControl = train_control
)

model
model$finalModel
# RmSE 14.28206