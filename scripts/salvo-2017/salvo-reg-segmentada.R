# Ozônio - Salvo, 2017
# Regressão linear

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(caret)
library(segmented)
library(patchwork)

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

# Validação cruzada

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
  p1 = c(0.19, 0.2, 0.21),
  p2 = c(0.59, 0.6, 0.61)
  #p3 = seq(0.57, 0.63, 0.01)
) %>% 
  split(x = ., seq(nrow(.))) %>% 
  map(as.numeric)

set.seed(5893524)

resultados <- map_dfr(
  pontos,
  validacao_cruzada,
  formula = formula,
  df = df_model
)

#saveRDS(resultados, file = "scripts/salvo-2017/seg-reg-res.rds")

resultados_ <- resultados %>% 
  bind_rows() %>% 
  mutate(pontos = map(pontos, paste, collapse = "-") %>% unlist)

resultados_ %>% 
  arrange(rmse)

# Melhor modelo (2 pontos)

# pontos: 0.21 - 0.6
# R2 = 0.705
# RMSE = 19.6


# Melhor modelo (3 pontos)

# pontos: 0.21 - 0.51 - 0.59
# R2 = 0.71
# RMSE = 19.4

set.seed(5893524)
model <- lm(formula, data = df_model)
seg_model <- segmented(model, seg.Z = ~share_gas, psi = c(0.21, 0.6))

summary(seg_model)

plot(seg_model)

seg_model %>% 
  broom::tidy() %>% 
  mutate(statistic = abs(statistic)) %>% 
  arrange(desc(statistic)) %>% 
  select(term, statistic)

# Gráfico 2 pontos

p1 <- df_model %>% 
  filter(siteid == 1) %>% 
  ggplot(aes(x = share_gas, y = -9)) +
  geom_point(size = 0.1) +
  geom_segment(
    x = 0.1395, 
    y = 10.72212, 
    xend = 0.5127, 
    yend = 9.898471,
    size = 0.2,
    color = "blue"
  ) +
  geom_segment(
    x = 0.5127, 
    y = 9.898471,
    xend = 0.5756, 
    yend = -7.890089,
    size = 0.2,
    color = "blue"
  ) +
  geom_segment(
    x = 0.5756, 
    y = -7.890089, 
    xend = 0.7552, 
    yend = 11.11033,
    size = 0.2,
    color = "blue"
  ) +
  theme_bw() +
  labs(
    x = "Proporção estimada de carros a gasolina", 
    y = "Efeito na concentração de ozônio"
  ) +
  coord_cartesian(ylim = c(-10, 13)) +
  ggtitle("2 pontos")

11.03 - 2.207*0.1395
11.03 - 2.207*0.5127

11.03 - 2.207 * 0.5756 -2.806e+02 * (0.5756 - 0.5127)
11.03 - 2.207 * 0.7552 - 2.806e+02 * (0.7552 -  0.5127) +
  3.886e+02 * (0.7552 - 0.5756)


# Gráfico 3 pontos

p2 <- df_model %>% 
  filter(siteid == 1) %>% 
  ggplot(aes(x = share_gas, y = -8)) +
  geom_point(size = 0.1) +
  geom_segment(
    x = 0.1395, 
    y = 13.70102, 
    xend = 0.2625, 
    yend = 3.4503,
    size = 0.1,
    color = "blue"
  ) +
  geom_segment(
    x = 0.2625, 
    y = 3.4503,
    xend = 0.5070, 
    yend = 11.898,
    size = 0.1,
    color = "blue"
  ) +
  geom_segment(
    x = 0.5070, 
    y = 11.898, 
    xend = 0.5751, 
    yend = -7.7471,
    size = 0.1,
    color = "blue"
  ) +
  geom_segment(
    x = 0.5751, 
    y = -7.7471, 
    xend = 0.7552, 
    yend = 10.6939,
    size = 0.1,
    color = "blue"
  ) +
  theme_bw() +
  labs(
    x = "Proporção estimada de carros a gasolina", 
    y = "Efeito na concentração de ozônio"
  ) +
  coord_cartesian(ylim = c(-10, 14)) +
  ggtitle("3 pontos")

p1 + p2 
ggsave(filename = "text/figuras/cap-comb-seg-reg-plot.pdf", 
       width = 6, height = 4)

25.32680 - 83.33890*0.1395
25.32680 - 83.33890*0.2625

25.32680 - 83.33890*0.5070 + 117.88960*(0.5070-0.2625)

25.32680 - 83.33890*0.5751 + 117.88960*(0.5751-0.2625) - 
  323.02512*(0.5751-0.5070)

25.32680 - 83.33890*0.7552 + 117.88960*(0.7552-0.2625) - 
  323.02512*(0.7552-0.5070) + 390.86760*(0.7552-0.5751)