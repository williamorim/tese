# Libraries ---------------------------------------------------------------

library(tidyverse)
library(caret)
library(recipes)
library(forecast)

# Dados -------------------------------------------------------------------

df_model <- read_rds("data/artaxo-salvo-geiger/data-asg-model.rds")

source("scripts/salvo-2017/salvo-utils.R")

# Formulas ----------------------------------------------------------------

formula <- df_model %>%
  select(
    -siteid, -stationname,
    -date, -o3_mass_conc, -dayofweek,
    -starts_with("congestion"),
    -dv_kmregion_am_18_max, -dv_kmcity_am_80_max,
    -pp, -dv_pp_20_150,
    -dv_sun_reg,
    -year, -month, -day, -dv_weekday_regular, -dv_yearendvacation,
    -week, -trend, 
  ) %>%
  names() %>%
  stringr::str_c(collapse = " + ") %>%
  stringr::str_c("residuos ~ ", .) %>%
  as.formula()

# Model -------------------------------------------------------------------

# Function to train the models

train_model <- function(df_model, formula, train_control) {
  
  set.seed(5893524)
  
  serie <- ts(
    df_model$o3_mass_conc,
    start = c(2008, 183),
    end = c(2013, 152),
    frequency = 243
  )
  
  arima_fit <- auto.arima(serie, max.order = 5, lambda = "auto")
  
  df_model <- df_model %>% 
    mutate(residuos = arima_fit$residuals)
  
  glm_fit <- train(
    form = formula,
    data = na.omit(df_model),
    method = "glm",
    trControl = train_control
  )
  
  list(arima_fit = arima_fit, glm_fit = glm_fit)
  
}

# Fits

train_control <- trainControl(method="cv", number = 5)

ajustes <- df_model %>%
  group_by(stationname) %>% 
  nest() %>%
  mutate(fit = map(
    data,
    train_model, 
    formula = formula, 
    train_control = train_control
  )) %>% 
  select(-data)

# Resultados

extrai_resultados <- function(ajuste, preditor) {
  
  ajuste <- ajuste$glm_fit
  
  imp <-  ajuste %>% 
    varImp() %>% 
    .$importance %>% 
    rownames_to_column("var") %>%
    mutate(n = row_number(desc(Overall))) %>% 
    filter(var == preditor) %>% 
    .$n
  
  estimativas <- ajuste %>% 
    .$finalModel %>% 
    broom::tidy() %>% 
    filter(term == preditor) %>% 
    magrittr::extract(c("estimate", "p.value")) %>% 
    as.list()
  
  tibble(
    RMSE = ajuste$results$RMSE,
    R2 = ajuste$results$Rsquared,
    MAE = ajuste$results$MAE,
    varImp = imp,
    coeficiente = estimativas$estimate,
    valor_p = estimativas$p.value
  ) %>% 
    mutate(
      variacao = round((exp(coeficiente*10) - 1)*100, 2)
    )
  
}

resultados <- ajustes %>% 
  mutate(
    resultados = map(fit, extrai_resultados, preditor = "share_gas")
  ) %>% 
  select(-fit) %>% 
  unnest(resultados)

resultados %>% View

summary(ajustes$fit[[12]]$glm_fit)

# stationname        valor_p
# <chr>                <dbl>
#   1 Diadema            0.665  
# 2 Dom Pedro II       0.0426 *
# 3 Ibirapuera         0.576  
# 4 IPEN               0.144  
# 5 Maua               0.00922 *
# 6 Mooca              0.138  
# 7 Nossa Senhora do O 0.00655 *
# 8 Parelheiros        0.0435 *
# 9 Pinheiros          0.0777 -
# 10 Santana            0.0229 *
# 11 Santo Andre 1      0.0329 *
# 12 Sao Caetano do Sul 0.240 
