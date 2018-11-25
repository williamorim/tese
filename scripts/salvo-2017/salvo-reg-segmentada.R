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

ajustar_modelo <- function(formula, df, pontos) {
  model <- lm(formula, data = df)
  segmented(model, seg.Z = ~share_gas, psi = pontos)
}

ajustar_modelo_segura <- purrr::possibly(ajustar_modelo, NA)

calcular_erro <- function(modelo, df) {
  
  df %>% 
    mutate(
      pred = predict(modelo, df)
    ) %>% 
    filter(!is.na(o3_mass_conc), !is.na(pred)) %>% 
    summarise(
      sqr = sum((o3_mass_conc - pred)^2),
      sqt = sum((o3_mass_conc - mean(o3_mass_conc))^2),
      rmse = sqrt(sqr/length(pred)),
      r2 = 1 - sqr/sqt
    ) %>% 
    select(rmse, r2)
  
}

validacao_cruzada <- function(pontos, formula, df, k = 5) {
  
  df$fold <- sample.int(n = k, size = nrow(df), replace = TRUE)
  
  erros <- NULL
  
  for(i in 1:k) {
    
    df_treino <- filter(df, fold != i)
    df_teste <- filter(df, fold == i)
    
    modelo <- ajustar_modelo_segura(formula, df_treino, pontos)
    
    if(!is.na(modelo)) {
      erros <- bind_rows(
        erros,
        calcular_erro(modelo, df_teste)
      )
    }
    
  }
  
  print(
    paste("Modelo para os pontos", paste(pontos, collapse = "-"), "finalizado!")
  )
  
  erros %>% 
    summarise(
      rmse = mean(rmse),
      r2 = mean(r2)
    )
  
}

pontos <- expand.grid(
  p1 = seq(0.17, 0.23, 0.01),
  p2 = seq(0.47, 0.53, 0.01),
  p3 = seq(0.57, 0.63, 0.01)
) %>% 
  split(x = ., seq(nrow(.))) %>% 
  map(as.numeric)

set.seed(5893524)

resultados <- map(
  pontos,
  validacao_cruzada,
  formula = formula,
  df = df_model
)

model <- lm(formula, data = df_model)
seg_model <- segmented(model, seg.Z = ~share_gas, psi = c(0.2, 0.5, 0.6))

summary(seg_model)

plot(seg_model)

# RMSE: 19.74
# MAE: 14.96
# % var: 70.27%  
# share_gas imp: 15º
