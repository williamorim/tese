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
    mutate(residuos = arima_fit$residuals) %>% 
    slice(-1)
  
  gam_fit <- train(
    form = as.formula(formula),
    data = na.omit(df_model),
    method = "gam",
    trControl = train_control
  )
  
  list(arima_fit = arima_fit, gam_fit = gam_fit)
  
}

# Fits

train_control <- trainControl(method = "cv", number = 5)

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
  
  ajuste <- ajuste$gam_fit
  
  best <- ajuste$bestTune
  
  ajuste$results <- filter(
    ajuste$results, 
    select == best$select, 
    method == best$method
  )
  
  imp <-  ajuste %>% 
    varImp() %>% 
    .$importance %>% 
    rownames_to_column("var") %>%
    mutate(n = row_number(desc(Overall))) %>% 
    filter(var == preditor) %>% 
    .$n
  
  valor_p <- ajuste %>% 
    .$finalModel %>% 
    broom::tidy() %>% 
    filter(str_detect(term, preditor)) %>% 
    magrittr::extract(c("p.value")) %>% 
    as.list()
  
  res = tibble(
    RMSE = ajuste$results$RMSE,
    R2 = ajuste$results$Rsquared,
    MAE = ajuste$results$MAE,
    varImp = imp,
    valor_p = valor_p
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
# temp max
# RMSE: 0.0001157788
# R2: 0.1247502
# varImp: 5
# valor-p: 0.008892

summary(ajustes$fit[[4]]$gam_fit)

gam_plot(
  ajustes$fit[[4]]$gam_fit$finalModel, 
  ajustes$fit[[4]]$gam_fit$finalModel$smooth[[1]],
  xlab = ajustes$fit[[4]]$gam_fit$finalModel$smooth[[1]]$term
)

predicao <- df_model %>% 
  mutate(
    pred_arima = forecast(
      ajustes$fit[[4]]$arima_fit$x,
      model = ajustes$fit[[4]]$arima_fit
    )$fitted
  ) %>%
  na.omit() %>% 
  mutate(
    pred_gam = predict(ajustes$fit[[4]]$gam_fit, newdata = na.omit(df_model)),
    pred = pred_gam + pred_arima
  ) %>% 
  slice(-1)

predicao %>%
  ggplot(aes(n_mortes_geral, pred)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "blue")

predicao %>% 
  summarise(RMSE = sqrt(mean((pred - n_mortes_geral)^2)))

# RMSE final: 37.3

# Idosos

resultados %>% 
  filter(grupo == "n_mortes_idosos") %>% 
  arrange(RMSE) %>% 
  View

# Melhor resultado:
# temp var
# RMSE: 0.0001268263
# R2: 0.05765525
# varImp: 3
# valor-p: 0.0430469618426295

summary(ajustes$fit[[5]]$gam_fit)

predicao <- df_model %>% 
  mutate(
    pred_arima = forecast(
      ajustes$fit[[5]]$arima_fit$x,
      model = ajustes$fit[[5]]$arima_fit
    )$fitted
  ) %>%
  na.omit() %>% 
  mutate(
    pred_gam = predict(ajustes$fit[[5]]$gam_fit, newdata = na.omit(df_model)),
    pred = pred_gam + pred_arima
  ) %>% 
  slice(-1)

predicao %>%
  ggplot(aes(n_mortes_idosos, pred)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "blue")

predicao %>% 
  summarise(RMSE = sqrt(mean((pred - n_mortes_idosos)^2)))

# RMSE final: 28.7

# Crianças

resultados %>% 
  filter(grupo == "n_mortes_criancas") %>% 
  arrange(RMSE) %>% 
  View

# Melhor resultado:
# temp var
# RMSE: 0.5155650
# R2: 0.003999033
# varImp: 4
# valor-p: 0.497194064401343

summary(ajustes$fit[[9]]$gam_fit)

predicao <- df_model %>% 
  mutate(
    pred_arima = forecast(
      ajustes$fit[[9]]$arima_fit$x,
      model = ajustes$fit[[9]]$arima_fit
    )$fitted
  ) %>%
  na.omit() %>% 
  mutate(
    pred_gam = predict(ajustes$fit[[9]]$gam_fit, newdata = na.omit(df_model)),
    pred = pred_gam + pred_arima
  )

predicao %>%
  ggplot(aes(n_mortes_criancas, pred)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "blue")

predicao %>% 
  summarise(RMSE = sqrt(mean((pred - n_mortes_criancas)^2)))

# RMSE final: 5.04
