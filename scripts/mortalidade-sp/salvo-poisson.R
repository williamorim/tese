# Libraries ---------------------------------------------------------------

library(tidyverse)
library(caret)
library(recipes)
library(patchwork)

# Dados -------------------------------------------------------------------

df_model <- read_rds("data/datasus-sim/model_mort_diaria_salvo.rds")
# df_share_bs <- read_rds("data/artaxo-salvo-geiger/dados_gas_200_bs.rds")

source("scripts/salvo-2017/salvo-utils.R")

# Formulas ----------------------------------------------------------------

cria_formula <- function(mort, temp, sazon) {
  
  padrao <- "share_gas + hm + trend + dayofweek + dv_workday"
  
  
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
    method = "glm",
    family = poisson(link = "log"),
    trControl = train_control
  )
  
}

# Fits

train_control <- trainControl(method="cv", number = 5)

ajustes <- map(
  formulas$formula,
  train_model,
  df_model = filter(df_model, week != "26"),
  train_control = train_control
)

# Resultados

extrai_resultados <- function(ajuste, preditor) {
  
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

resultados <- map_dfr(
  ajustes,
  extrai_resultados,
  preditor = "share_gas"
) %>% 
  bind_cols(formulas, .)


# Idosos

resultados %>% 
  filter(mortalidade == "n_mortes_idosos") %>% 
  arrange(RMSE) %>% 
  View


# Melhor resultado:
# temp média
# month
# RMSE: 15.55117
# R2: 0.4045783
# varImp: 13
# variação (+10% share): 91.01%
# valor-p: < 000.1

# Crianças

resultados %>% 
  filter(mortalidade == "n_mortes_criancas") %>% 
  arrange(RMSE) %>% 
  View


# Melhor resultado:
# temp média
# month
# RMSE: 3.24
# R2: 0.00720
# varImp: 11
# valor-p: 0.6101849


# Gráficos de resíduos

p_idosos <- pred_obs_plot2(
  df = df_model, 
  model = ajustes[[1]], 
  y = "n_mortes_idosos"
) +
  ggtitle("Idosos")

p_criancas <- pred_obs_plot2(
  df = df_model, 
  model = ajustes[[2]], 
  y = "n_mortes_criancas"
) +
  ggtitle("Crianças")


p_idosos + p_criancas
ggsave(filename = "text/figuras/cap-mort-res-plot-glm.pdf", 
       width = 6, height = 4)



# ggplot(df_model) +
#   geom_boxplot(aes(x = season, y = share_gas))
# 
# 
# ggplot(df_model) +
#   geom_boxplot(aes(x = week, y = n_mortes_geral, fill = month))


p1 <- ggplot(df_model) +
  geom_boxplot(aes(x = month, y = n_mortes_geral)) +
  labs(x = "Mês", y = "Número de mortes (geral)") +
  theme_bw()

p2 <- ggplot(df_model) +
  geom_boxplot(aes(x = month, y = share_gas))  +
  labs(x = "Mês", y = "Proporção de carros a gasolina") +
  theme_bw()

p1 + p2
ggsave(filename = "text/figuras/cap-mort-boxplot-share.pdf", 
       width = 6, height = 4)
