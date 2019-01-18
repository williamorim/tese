# Ozônio - Salvo, 2017
# Regressão linear


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(caret)

# Dados -------------------------------------------------------------------

df_model <- read_rds("data/artaxo-salvo-geiger/data-asg-model-nox.rds")

source("scripts/salvo-2017/salvo-utils.R")

# Formula -----------------------------------------------------------------

formula <- df_model %>%
  select(
    -siteid,
    -date, -o3_mass_conc, -NOx, -indice, -dayofweek,
    -starts_with("congestion"),
    -dv_kmregion_am_18_max, -dv_kmcity_am_80_max,
    -pp, -dv_pp_20_150,
    -dv_sun_reg,
    -year, -month, -day, -dv_weekday_regular, -dv_yearendvacation,
  ) %>%
  names() %>%
  stringr::str_c(collapse = " + ") %>%
  stringr::str_c("log(indice) ~ ", .) %>%
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
model$finalModel %>% summary
varImp(model)

# RMSE: 19.74
# MAE: 14.96
# % var: 70.27%  
# share_gas imp: 15º

pred_obs_plot(
  obs = log(na.omit(df_model)$indice),
  pred = predict(model, newdata = na.omit(df_model))
)
# ggsave(
#   filename = "text/figuras/cap-comb-lin-reg-pred-obs-plot.pdf", 
#   width = 6, 
#   height = 4
# )


