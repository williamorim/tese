# Ozônio - Salvo, 2017
# Regressão linear


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(caret)
library(segmented)

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
    -year, -month, -day, -dv_weekday_regular, -dv_yearendvacation
  ) %>%
  names() %>%
  stringr::str_c(collapse = " + ") %>%
  stringr::str_c("o3_mass_conc ~ ", .) %>%
  stringr::str_c("+ trend*stationname") %>%
  stringr::str_c("+ dv_beltway_open*stationname") %>%
  as.formula()

# Model -------------------------------------------------------------------

model <- lm(formula, data = df_model)
seg_model <- segmented(model, seg.Z = ~share_gas, psi = c(0.15, 0.5))

summary(seg_model)

plot(seg_model)

# RMSE: 19.74
# MAE: 14.96
# % var: 70.27%  
# share_gas imp: 15º
