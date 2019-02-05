# Libraries ---------------------------------------------------------------

library(tidyverse)
library(caret)
library(recipes)
library(patchwork)

# Dados -------------------------------------------------------------------

df_model <- read_rds("data/datasus-sim/model_mort_diaria_salvo.rds")
#df_share_bs <- read_rds("data/artaxo-salvo-geiger/dados_gas_200_bs.rds")

source("scripts/salvo-2017/salvo-utils.R")

# Formulas ----------------------------------------------------------------

cria_formula <- function(mort, temp, sazon) {
  
  padrao <- "o3_mass_conc + NOx + hm + trend + dayofweek + dv_workday"
  
  
  padrao %>% 
    str_c(temp, sep = " + ") %>% 
    str_c(sazon, sep = " + ") %>% 
    str_c(mort, ., sep = " ~ ")
  
}

formulas <- expand.grid(
  mortalidade = c("n_mortes_idosos", "n_mortes_criancas"),
  #temperatura = c("tp", "tp_var", "tp_min", "tp_max"),
  temperatura = "tp",
  #sazonalidade = c("month", "season")
  sazonalidade = "month"
) %>% 
  as.tibble() %>% 
  mutate(formula = cria_formula(mortalidade, temperatura, sazonalidade))

# Model -------------------------------------------------------------------

# Function to train the models

train_model <- function(formula, df_model, train_control) {
  
  set.seed(5893524)
  
  train(
    form = as.formula(formula),
    data = na.omit(df_model),
    method = "gam",
    family = poisson(link = "log"),
    trControl = train_control
  )
  
}

# Fits

train_control <- trainControl(method="cv", number = 5)

ajustes <- map(
  formulas$formula,
  train_model,
  df_model = df_model,
  train_control = train_control
)

# Resultados

extrai_resultados <- function(ajuste, preditor) {
  
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
    filter(var %in% preditor) %>% 
    arrange(var) %>% 
    .$n
  
  aux <- paste0("s(", preditor, ")")
  
  valor_p <- ajuste %>% 
    .$finalModel %>% 
    broom::tidy() %>% 
    filter(term %in% aux) %>% 
    arrange(term) %>%
    .$p.value
  
  res = tibble(
    predictor = preditor,
    RMSE = ajuste$results$RMSE,
    R2 = ajuste$results$Rsquared,
    MAE = ajuste$results$MAE,
    varImp = imp,
    valor_p = valor_p
  )
  
}

resultados <- map_dfr(
  ajustes,
  extrai_resultados,
  preditor = c("NOx", "o3_mass_conc")
) %>% 
  bind_cols(slice(formulas, c(1, 1, 2, 2)), .)

# Idosos

resultados %>% 
  filter(mortalidade == "n_mortes_idosos") %>% 
  arrange(RMSE) %>% 
  View

# Melhor resultado:
# temp média
# month
# RMSE: 15.02218
# R2: 0.4492839
# varImp O3: 23
# varImp NOX: 9
# valor-p O3: 1
# valor-p NOX: < 0.001

p_idosos_o3 <- gam_plot(
  ajustes[[1]]$finalModel, 
  ajustes[[1]]$finalModel$smooth[[3]],
  xlab = "Concentração de ozônio",
  ylab = "Efeito na mortalidade"
) +
  ggtitle("Ozônio (Idosos)")

p_idosos_nox <- gam_plot(
  ajustes[[1]]$finalModel, 
  ajustes[[1]]$finalModel$smooth[[2]],
  xlab = "Concentração de NOx",
  ylab = "Efeito na mortalidade"
) +
  ggtitle("NOx (Idosos)")

# Crianças

resultados %>% 
  filter(mortalidade == "n_mortes_criancas") %>% 
  arrange(RMSE) %>% 
  View

# Melhor resultado:
# temp média
# month
  # RMSE: 3.228937
# R2: 0.01212866
# varImp O3: 19
# varImp NOX: 21
# valor-p O3: 0.80
# valor-p NOX: 0.91

p_criancas_o3 <- gam_plot(
  ajustes[[2]]$finalModel, 
  ajustes[[2]]$finalModel$smooth[[3]],
  xlab = "Concentração de ozônio",
  ylab = "Efeito na mortalidade"
) +
  ggtitle("Ozônio (Crianças)")

p_criancas_nox <- gam_plot(
  ajustes[[2]]$finalModel, 
  ajustes[[2]]$finalModel$smooth[[2]],
  xlab = "Concentração de NOx",
  ylab = "Efeito na mortalidade"
) +
  ggtitle("NOx (Crianças)")


patchwork::wrap_plots(
  p_idosos_o3, 
  p_idosos_nox,
  p_criancas_o3,
  p_criancas_nox, 
  ncol = 2
)

ggsave(filename = "text/figuras/cap-mort-gam-plot-poisson-ozonio.pdf", 
       width = 8, height = 4)
