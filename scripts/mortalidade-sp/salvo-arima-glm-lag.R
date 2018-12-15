# Libraries ---------------------------------------------------------------

library(tidyverse)
library(caret)
library(recipes)
library(forecast)

# Dados -------------------------------------------------------------------

df_model <- read_rds("data/datasus-sim/model_mort_diaria_salvo.rds")
# df_share_bs <- read_rds("data/artaxo-salvo-geiger/dados_gas_200_bs.rds")

source("scripts/salvo-2017/salvo-utils.R")

# Formulas ----------------------------------------------------------------

formula <- "residuos ~ share_gas + tp_var + hm + dayofweek + dv_workday"

# Model -------------------------------------------------------------------

# Function to train the models

train_model <- function(mortalidade, lag, df_model, formula, train_control) {
  
  set.seed(5893524)
  
  df_model <- df_model %>% 
    gather(grupo, n_mortes, starts_with("n_mortes")) %>%
    filter(grupo == mortalidade)
  
  serie <- ts(
    df_model$n_mortes,
    start = c(2008, 305),
    end = c(2013, 152),
    frequency = 365
  )
  
  arima_fit <- auto.arima(serie, max.order = 5, lambda = "auto")
  
  if(lag > 0) {
    df_model <- df_model %>% 
      mutate(
        share_gas = lag(share_gas, lag)
      )
  }
  
  df_model <- df_model %>% 
    mutate(
      residuos = arima_fit$residuals
    )
  
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

ajustes <- expand.grid(
  mortalidade = c("n_mortes_idosos"),
  lag = 8:21
) %>% 
  as.tibble() %>% 
  mutate(
    fit = map2(
      mortalidade,
      lag,
      train_model, 
      df_model = df_model,
      formula = formula, 
      train_control = train_control
    )
  )

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

# Idosos

resultados %>% 
  filter(mortalidade == "n_mortes_idosos") %>% 
  arrange(RMSE) %>% 
  # select(mortalidade, lag, RMSE, R2, valor_p) %>% 
  # mutate(mortalidade = "") %>% 
  # mutate(RMSE = as.character(round(RMSE, 7))) %>% 
  # mutate_if(is.numeric, funs(round), digits = 2) %>% 
  # knitr::kable(format = "latex")
  View

# Melhor resultado:
# temp var
# RMSE: 0.0001265393
# R2: 0.05441715
# varImp: 9
# valor-p: 0.6262879
# lag: 7

summary(ajustes$fit[[27]]$glm_fit)

predicao <- df_model %>% 
  mutate(
    pred_arima = forecast(
      ajustes$fit[[15]]$arima_fit$x,
      model = ajustes$fit[[15]]$arima_fit
    )$fitted
  ) %>%
  na.omit() %>% 
  mutate(
    pred_glm = predict(ajustes$fit[[15]]$glm_fit, newdata = na.omit(df_model)),
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
  filter(mortalidade == "n_mortes_criancas") %>% 
  arrange(RMSE) %>% 
  View

# Melhor resultado:
# temp var
# RMSE: 0.5143595
# R2: 0.002388130
# varImp: 2
# valor-p: 0.4252288

summary(ajustes$fit[[2]]$glm_fit)

predicao <- df_model %>% 
  mutate(
    pred_arima = forecast(
      ajustes$fit[[2]]$arima_fit$x,
      model = ajustes$fit[[2]]$arima_fit
    )$fitted
  ) %>%
  na.omit() %>% 
  mutate(
    pred_glm = predict(ajustes$fit[[2]]$glm_fit, newdata = na.omit(df_model)),
    pred = pred_glm + pred_arima
  )

predicao %>%
  ggplot(aes(n_mortes_criancas, pred)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "blue")

predicao %>% 
  summarise(RMSE = sqrt(mean((pred - n_mortes_criancas)^2)))

# RMSE final: 5.03
