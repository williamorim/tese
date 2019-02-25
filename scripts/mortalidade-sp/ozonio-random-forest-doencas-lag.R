# Libraries ---------------------------------------------------------------

library(tidyverse)
library(caret)
library(recipes)
library(iml)
library(patchwork)

# Dados -------------------------------------------------------------------

df_model <- read_rds("data/datasus-sim/model_mort_diaria_salvo.rds")

source("scripts/salvo-2017/salvo-utils.R")

# Formula -----------------------------------------------------------------

cria_formula <- function(mort) {
  
  padrao <- "o3_mass_conc + NOx + hm + tp + trend + dayofweek + dv_workday + month"
  
  padrao %>% 
    str_c(mort, ., sep = " ~ ") %>% 
    as.formula()
  
}

cria_receita <- function(df_model, formula) {
  
  rec <- recipe(formula = formula, data = df_model) %>% 
    step_dummy(dayofweek, month, one_hot = TRUE)
  
}

receitas <- expand.grid(
  mortalidade = c("n_mortes_idosos2"),
  lag = seq(1, 30, 1)
) %>% 
  as.tibble() %>% 
  mutate(
    formula = map(mortalidade, cria_formula),
    receita = map(formula, cria_receita, df_model = df_model)
  )

# Model -------------------------------------------------------------------

# Function to train the models

train_model <- function(receita, lag, df_model, train_control, tuning_grid) {
  
  set.seed(5893524)
  
  df_model <- df_model %>% 
    mutate(
      o3_mass_conc = lag(o3_mass_conc, lag),
      NOx = lag(NOx, lag)
    )
  
  train(
    receita,
    data = na.omit(df_model),
    method = "ranger",
    trControl = train_control, 
    tuneGrid = tuning_grid,
    importance = 'impurity'
  )
  
}

# Fits

train_control <- trainControl(method="cv", number = 5)

tuning_grid <- expand.grid(
  splitrule = "variance",
  mtry = 20,
  min.node.size = 3
)

ajustes <- map2(
  receitas$receita,
  receitas$lag,
  train_model,
  df_model = filter(df_model),
  train_control = train_control,
  tuning_grid = tuning_grid
)

# Resultados

extrai_resultados <- function(ajuste, preditor) {
  
  imp <-  ajuste %>% 
    varImp() %>% 
    .$importance %>% 
    rownames_to_column("var") %>%
    mutate(n = row_number(desc(Overall))) %>% 
    filter(var %in% preditor) %>% 
    arrange(var) %>% 
    .$n
  
  tibble(
    var = preditor,
    RMSE = ajuste$results$RMSE,
    R2 = ajuste$results$Rsquared,
    MAE = ajuste$results$MAE,
    varImp = imp
  )
  
}

resultados <- map_dfr(
  ajustes,
  extrai_resultados,
  preditor = c("NOx", "o3_mass_conc")
) %>% 
  bind_cols(slice(receitas, rep(1:30, rep(2, 30))), .) %>% 
  select(-formula, -receita)

# Idosos

resultados %>% 
  filter(mortalidade == "n_mortes_idosos2") %>% 
  arrange(RMSE, lag, var) %>% 
  View

resultados %>% 
  filter(mortalidade == "n_mortes_idosos2") %>% 
  arrange(RMSE) %>% 
  top_n(10, -RMSE) %>% 
  mutate(blank = "", RMSE = round(RMSE, 2)) %>% 
  select(blank, lag, RMSE, R2) %>% 
  mutate(R2 = scales::percent(R2), blank2 = blank) %>% 
  knitr::kable(format = "latex")


# Interpretação ------------------------------------------------------------

df_model_ <- df_model %>% 
  mutate(
    o3_mass_conc = lag(o3_mass_conc, 1),
    NOx = lag(NOx, 1)
  ) %>% 
  na.omit()

df_train <- receitas$receita[[1]] %>%
  prep(df_model_) %>% 
  bake(na.omit(df_model_))


X <- df_train %>% 
  select(-n_mortes_idosos2) %>% 
  as.matrix()

train_control <- trainControl(method="cv", number = 5)

tuning_grid <- expand.grid(
  splitrule = "variance",
  mtry = 20,
  min.node.size = 3
)

model <- train(
  x = X,
  y = df_train$n_mortes_idosos2,
  method = "ranger",
  trControl = train_control,
  tuneGrid = tuning_grid,
  importance = 'impurity'
)

predictor <- Predictor$new(model, data = df_train, y = df_train$n_mortes_idosos2)

# PDP O3
pdp <- FeatureEffect$new(predictor, feature = "o3_mass_conc", method = "pdp", 
                         grid.size = 20)
p_pdp_o3 <- pdp$plot() + 
  theme_bw() +
  labs(x = expression(paste(O[3], " (", mu, "g/", m^3, ")"))) +
  scale_y_continuous(name = "Mortalidade diária estimada") +
  ggtitle("PDP")

# ALE O3
ale <- FeatureEffect$new(predictor, feature = "o3_mass_conc", grid.size = 15)
p_ale_o3 <- ale$plot() + 
  theme_bw() +
  labs(x = expression(paste(O[3], " (", mu, "g/", m^3, ")"))) +
  scale_y_continuous(name = "Diferença em relação à predição média") +
  ggtitle("ALE")

# PDP NOX
pdp <- FeatureEffect$new(predictor, feature = "NOx", method = "pdp", 
                         grid.size = 20)
p_pdp_nox <- pdp$plot() + 
  theme_bw() +
  labs(x = expression(paste(NO[x], " (", mu, "g/", m^3, ")"))) +
  scale_y_continuous(name = "Mortalidade diária estimada") +
  ggtitle("PDP")

# ALE NOX
ale <- FeatureEffect$new(predictor, feature = "NOx", grid.size = 15)
p_ale_nox <- ale$plot() + 
  theme_bw() +
  labs(x = expression(paste(NO[x], " (", mu, "g/", m^3, ")"))) +
  scale_y_continuous(name = "Diferença em relação à predição média") +
  ggtitle("ALE")

patchwork::wrap_plots(
  p_pdp_o3, 
  p_ale_o3,
  p_pdp_nox,
  p_ale_nox, 
  ncol = 2
)

ggsave(
  filename = "text/figuras/cap-mort-ozonio-rf-graficos-iml-lag-doencas.pdf", 
  width = 7, height = 7
)


# Gráfico de correçalação cruzada ------------------------------------------

calculate_ccf <- function(data, lag.max = 60) {
  
  ccf(x = data$x, y = data$y, lag.max = lag.max, plot = FALSE)
  
}

make_ccf_df <- function(ccf) {
  
  tibble(
    lag = as.numeric(ccf$lag), 
    cc = as.numeric(ccf$acf),
    liminf = -2/sqrt(ccf$n.used),
    limsup = 2/sqrt(ccf$n.used)
  )
  
}

df_model %>% 
  select(x = o3_mass_conc, y = n_mortes_idosos2) %>%
  na.exclude() %>% 
  calculate_ccf(lag.max = 60) %>% 
  make_ccf_df() %>% 
  ggplot(aes(x = lag, ymin = min(0, abs(cc)), ymax = cc)) +
  geom_hline(aes(yintercept = liminf), linetype = 3) +
  geom_hline(aes(yintercept = limsup), linetype = 3) +
  geom_linerange() +
  theme_bw() +
  labs(x = "Defasagem", y = "Função de correlação cruzada")
ggsave("text/figuras/cap-mort-ccf-ozonio-doencas-lag.pdf", width = 12, height = 6)

