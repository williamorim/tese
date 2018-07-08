# Ozônio - Salvo, 2017
# Regressão linear


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(caret)

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
    -year, -month, -day, -dv_weekday_regular, -dv_yearendvacation, -dv_o3
  ) %>%
  names() %>%
  stringr::str_c(collapse = " + ") %>%
  stringr::str_c("o3_mass_conc ~ ", .) %>%
  stringr::str_c("+ trend*stationname") %>%
  stringr::str_c("+ dv_beltway_open*stationname") %>%
  as.formula()

# Model -------------------------------------------------------------------

set.seed(5893524)

# 5-fold cross-validation
train_control <- trainControl(method="cv", number = 10)

model <- train(
  form = formula, 
  data = na.omit(df_model), 
  method = "lm", 
  trControl = train_control
)

model
model$finalModel
varImp(model)

# RMSE: 19.74
# MAE: 14.96
# % var: 70.27%  
# share_gas imp: 15º


