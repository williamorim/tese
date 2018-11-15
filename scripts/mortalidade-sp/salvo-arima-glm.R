# Libraries ---------------------------------------------------------------

library(tidyverse)
library(caret)
library(recipes)
library(forecast)

# Dados -------------------------------------------------------------------

df_model <- read_rds("data/datasus-sim/model_mort_diaria_salvo.rds")
df_share_bs <- read_rds("data/artaxo-salvo-geiger/dados_gas_200_bs.rds")

source("scripts/salvo-2017/salvo-utils.R")

# Formulas ----------------------------------------------------------------

formula <- "residuos ~ share_gas + tp + hm + dayofweek + dv_workday"

# Model -------------------------------------------------------------------

# Function to train the models

train_model <- function(df_model, formula, train_control) {
  
  set.seed(5893524)
  
  serie <- ts(
    df_model$n_mortes,
    start = c(2008, 305),
    end = c(2013, 152),
    frequency = 365
  )
  
  arima_fit <- auto.arima(serie, max.order = 5, lambda = "auto")
  
  df_model <- df_model %>% 
    mutate(residuos = arima_fit$residuals)
  
  glm_fit <- train(
    form = as.formula(formula),
    data = na.omit(df_model),
    method = "glm",
    trControl = train_control
  )
  
  list(arima_fit = arima_fit, glm_fit = glm_fit)
  
}

# Fits

train_control <- trainControl(method="cv", number = 5)

ajustes <- df_model %>% 
  gather(grupo, n_mortes, starts_with("n_mortes")) %>%
  gather(tipo_tp, tp, starts_with("tp")) %>%
  group_by(grupo, tipo_tp) %>% 
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

# Geral

resultados %>% 
  filter(grupo == "n_mortes_geral") %>% 
  arrange(RMSE) %>% 
  View

# Melhor resultado:
# temp var
# RMSE: 0.0001164661
# R2: 0.1147159
# varImp: 7
# valor-p: 0.39975069

summary(ajustes$fit[[7]]$glm_fit)

predicao <- df_model %>% 
  mutate(
    pred_arima = forecast(
      ajustes$fit[[7]]$arima_fit$x,
      model = ajustes$fit[[7]]$arima_fit
    )$fitted
  ) %>%
  na.omit() %>% 
  mutate(
    pred_glm = predict(ajustes$fit[[7]]$glm_fit, newdata = na.omit(df_model)),
    pred = pred_glm + pred_arima
  )

predicao %>%
  ggplot(aes(n_mortes_geral, pred)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "blue")

predicao %>% 
  summarise(RMSE = sqrt(mean((pred - n_mortes_geral)^2)))

# RMSE final: 37.6

# Idosos

resultados %>% 
  filter(grupo == "n_mortes_idosos") %>% 
  arrange(RMSE) %>% 
  View

# Melhor resultado:
# temp var
# RMSE: 0.0001295715
# R2: 0.04988313
# varImp: 8
# valor-p: 0.28283413

summary(ajustes$fit[[8]]$glm_fit)

predicao <- df_model %>% 
  mutate(
    pred_arima = forecast(
      ajustes$fit[[8]]$arima_fit$x,
      model = ajustes$fit[[8]]$arima_fit
    )$fitted
  ) %>%
  na.omit() %>% 
  mutate(
    pred_glm = predict(ajustes$fit[[8]]$glm_fit, newdata = na.omit(df_model)),
    pred = pred_glm + pred_arima
  )

predicao %>%
  ggplot(aes(n_mortes_idosos, pred)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "blue")

predicao %>% 
  summarise(RMSE = sqrt(mean((pred - n_mortes_idosos)^2)))

# RMSE final: 28.8

# Crianças

resultados %>% 
  filter(grupo == "n_mortes_criancas") %>% 
  arrange(RMSE) %>% 
  View

# Melhor resultado:
# temp var
# RMSE: 0.5143595
# R2: 0.002388130
# varImp: 2
# valor-p: 0.4252288

summary(ajustes$fit[[9]]$glm_fit)

predicao <- df_model %>% 
  mutate(
    pred_arima = forecast(
      ajustes$fit[[9]]$arima_fit$x,
      model = ajustes$fit[[9]]$arima_fit
    )$fitted
  ) %>%
  na.omit() %>% 
  mutate(
    pred_glm = predict(ajustes$fit[[9]]$glm_fit, newdata = na.omit(df_model)),
    pred = pred_glm + pred_arima
  )

predicao %>%
  ggplot(aes(n_mortes_criancas, pred)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "blue")

predicao %>% 
  summarise(RMSE = sqrt(mean((pred - n_mortes_criancas)^2)))

# RMSE final: 5.03
