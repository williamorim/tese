# Ozônio - Salvo, 2017
# Regressão linear para cada estação


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(caret)

# Dados -------------------------------------------------------------------

df_model <- read_rds("data/artaxo-salvo-geiger/data-asg-model.rds")
df_share_bs <- read_rds("data/artaxo-salvo-geiger/dados_gas_200_bs.rds")

source("scripts/salvo-2017/salvo-utils.R")

# Formula -----------------------------------------------------------------

formula <- df_model %>%
  select(
    -siteid, -stationname,
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
  as.formula()

stations <- df_model %>% 
  select(stationname) %>% 
  distinct() %>% 
  flatten_chr()
  

# Model -------------------------------------------------------------------

train_model <- function(station, df, formula, train_control) {
  
  df <- df %>%
    filter(stationname == station)
  
  model <- train(
    form = formula, 
    data = na.omit(df), 
    method = "lm", 
    trControl = train_control
  )
  
  list(
    results = model$results, 
    best_fit = model$finalModel,
    imp_vars = varImp(model)
  )
  
}

set.seed(5893524)

# 5-fold cross-validation
train_control <- trainControl(method="cv", number = 10)

fits <- map(
  stations,
  train_model,
  df = df_model,
  formula = formula,
  train_control = train_control
)

#  Bootstrapping

fit_lm <- function(formula, data, vars, i) {
  
  fit <- lm(formula, data = data)
  as.numeric(fit$coefficients[vars])
  
}

boot <- function(station, df, df_share_bs, formula, fit_func) {
  
  df <- df %>%
    filter(stationname == station)
  
  coefs <- map_dbl(
    1:200,
    bootstrapping,
    df = df,
    df_share_bs = df_share_bs,
    formula = formula,
    vars = "share_gas",
    fit_func = fit_func
  )
  
  sd(coefs)
}

boot_se <- map_dbl(
  stations, 
  boot, 
  df = df_model,
  df_share_bs = df_share_bs,
  formula = formula,
  fit_func = fit_lm
)

# Tabela com resultados

get_impVars <- function(fit) {
  
  fit$imp_vars$importance %>% 
    rownames_to_column() %>% 
    arrange(desc(Overall)) %>% 
    slice(1:5) %>% 
    select(rowname) %>% 
    flatten_chr() %>% 
    str_c(collapse = ", ")
  
}

get_share <- function(fit) {
  
  as.numeric(fit$best_fit$coefficients["share_gas"])
  
}

tibble(
  modelo = stations,
  share = map_dbl(fits, get_share),
  se = boot_se,
  RMSE = map_dbl(fits, ~ .x[[1]]$RMSE),
  R2 = map_dbl(fits, ~ .x[[1]]$Rsquared),
  impVars = map_chr(fits, get_impVars)
) %>% 
  mutate(share = paste0(round(share, 2), " (", round(se, 2), ")")) %>% 
  select(-se) %>% 
  knitr::kable(format = "latex")


