library(tidyverse)
library(iml)
library(caret)
library(patchwork)
library(lime)

# Dados --------------------------------------------------------------------

dados <- read_rds("data/salvo-geiger/bd_original.rds")

dados <- dados %>% 
  filter(stationno == 1) %>% 
  mutate(date = lubridate::ymd(str_c(year, monthofyear, dayofmonth, sep = "-"))) %>% 
  group_by(date) %>% 
  summarise_at(vars(c("NOX", "tp", "hm")), funs(mean), na.rm = TRUE) %>% 
  na.omit()


# EDA ----------------------------------------------------------------------

# Temperatura e umidade vs NOX
dados %>% 
  gather(variavel, valor, tp, hm) %>%
  mutate(variavel = ifelse(variavel == "tp", "Temperatura", "Umidade")) %>% 
  ggplot() +
  geom_point(aes(x = valor, y = NOX), alpha = 0.5) +
  facet_wrap(~variavel, scales = "free") +
  theme_bw() +
  labs(x = "Média diária", y = "Média diária de NOx")
ggsave(
  filename = "text/figuras/cap-aprend-estat-tp-hm-nox.pdf", 
  width = 6, height = 4
)

# Temperatura vs umidade
dados %>% 
  ggplot() +
  geom_point(aes(y = tp, x = hm)) +
  theme_bw() +
  labs(y = "Temperatura (Celsius)", x = "Umidade (%)")
ggsave(
  filename = "text/figuras/cap-aprend-estat-tp-hm.pdf", 
  width = 6, height = 4
)

# Séries
dados %>% 
  gather(variavel, valor, NOX, tp, hm) %>%
  mutate(
    variavel = case_when(
      variavel == "hm" ~ "Umidade",
      variavel == "tp" ~ "Temperatura",
      TRUE ~ "NOx"
    )
  ) %>% 
  ggplot() +
  geom_line(aes(x = date, y = valor)) +
  geom_smooth(aes(x = date, y = valor)) +
  facet_wrap(~variavel, scales = "free") +
  labs(x = "Ano", y = "Média diária") +
  theme_bw()
ggsave(
  filename = "text/figuras/cap-aprend-estat-series-tp-hm-nox.pdf", 
  width = 6, height = 4
)

# Regressão linear ---------------------------------------------------------

ajuste_hm <- lm(NOX ~ hm, dados)
ajuste_hm_tp <- lm(NOX ~ hm + tp, dados)
ajuste_hm_tp_int <- lm(NOX ~ hm*tp, dados)

# Efeito modelo linear apenas com HM
p_hm <- dados %>% 
  mutate(
    efeito = ajuste_hm$coefficients[1] + ajuste_hm$coefficients[2]*hm
  ) %>%
  ggplot() +
  geom_line(aes(x = hm, y = efeito)) +
  labs(x = "Umidade", y = expression(paste(NO[x], " (", mu, "g/", m^3, ")"))) +
  ggtitle("Modelo (1)") +
  theme_bw()

# Efeito modelo linear sem interação
efeito_hm_tp <- function(ajuste, hm, tp) {
  efeito_hm_tp = ajuste_hm_tp$coefficients[1] + 
    ajuste_hm_tp$coefficients[2]*hm + 
    ajuste_hm_tp$coefficients[3]*mean(tp, na.rm = TRUE)
}

temperaturas <- seq(5, 35, 3) 

p_hm_tp <- map_dfc(
  temperaturas, 
  efeito_hm_tp, 
  ajuste = ajuste_hm_tp, 
  hm = dados$hm
) %>%
  `colnames<-`(temperaturas) %>% 
  bind_cols(dados) %>% 
  gather(Temperatura, efeito, `5`:`35`) %>%
  mutate(Temperatura = as.numeric(Temperatura)) %>% 
  ggplot() +
  geom_line(aes(x = hm, y = efeito, color = Temperatura, group = Temperatura)) +
  scale_color_gradient(low = "light blue", high = "orange") +
  labs(x = "Umidade", y = expression(paste(NO[x], " (", mu, "g/", m^3, ")"))) +
  theme_bw() +
  ggtitle("Modelo (2)")


# Efeito modelo linear com interação
efeito_hm_tp_int <- function(ajuste, hm, tp) {
  efeito_hm_tp_int = ajuste$coefficients[1] + 
    ajuste$coefficients[2]*hm + 
    ajuste$coefficients[3]*tp +
    ajuste$coefficients[4]*hm*tp
}

temperaturas <- seq(5, 35, 3) 

p_hm_tp_int <- map_dfc(
  temperaturas, 
  efeito_hm_tp_int, 
  ajuste = ajuste_hm_tp_int, 
  hm = dados$hm
) %>%
  `colnames<-`(temperaturas) %>% 
  bind_cols(dados) %>% 
  gather(Temperatura, efeito, `5`:`35`) %>%
  mutate(Temperatura = as.numeric(Temperatura)) %>% 
  ggplot() +
  geom_line(aes(x = hm, y = efeito, color = Temperatura, group = Temperatura)) +
  scale_color_gradient(low = "light blue", high = "orange") +
  labs(x = "Umidade", y = expression(paste(NO[x], " (", mu, "g/", m^3, ")"))) +
  theme_bw() +
  ggtitle("Modelo (3)")


wrap_plots(p_hm, p_hm_tp, p_hm_tp_int, nrow = 3)
ggsave(
  filename = "text/figuras/cap-aprend-estat-interp-lm.pdf", 
  width = 6, height = 8
)


# GAM ----------------------------------------------------------------------

ajuste_gam <- mgcv::gam(NOX ~ s(tp) + s(hm), data = dados)
plot(ajuste_gam)

# Random Forest ------------------------------------------------------------

ajuste <- train(
  form = NOX ~ tp + hm,
  data = dados[, -1],
  method = "ranger",
  importance = "impurity",
  tuneGrid = data.frame(splitrule = "extratrees", mtry = 1, min.node.size = 5)
)
ajuste
ajuste$finalModel$variable.importance
varImp(ajuste)

predictor <- Predictor$new(ajuste, data = dados[,c("hm", "tp")], y = dados$NOX)

# PDP
pdp <- FeatureEffect$new(predictor, feature = "hm", method = "pdp")
p_pdp <- pdp$plot() + 
  theme_bw() +
  labs(x = "Umidade") +
  scale_y_continuous(name = expression(paste(NO[x], " estimado (", mu, "g/", m^3, ")"))) +
  ggtitle("PDP")

# PDP + ICE
ice <- FeatureEffect$new(predictor, feature = "hm", method = "pdp+ice")
p_ice <- ice$plot() + 
  theme_bw() +
  labs(x = "Umidade") +
  scale_y_continuous(name = expression(paste(NO[x], " estimado (", mu, "g/", m^3, ")"))) +
  ggtitle("ICE")

# ALE
ale <- FeatureEffect$new(predictor, feature = "hm")
p_ale <- ale$plot() + 
  theme_bw() +
  labs(x = "Umidade") +
  scale_y_continuous(name = "Diferença em relação à predição média") +
  ggtitle("ALE")

p_pdp + p_ice + p_ale
ggsave(
  filename = "text/figuras/cap-aprend-estat-graficos-iml.pdf", 
  width = 7, height = 5
)

# LIME

make_explanation <- function(i, explainer, df, n_features = 2) {
  
  explanation <- explain(df[i,], explainer, n_features = n_features) %>% 
    mutate(feature_value = as.character(feature_value))
  
  if(i%%100 == 0) {
    print(paste("Another one bites the dust!", i))
  }
  
  explanation
  
}

explainer <- lime(
  dados[, -1], 
  ajuste
)

df_explain <- dados %>% 
  top_n(100, NOX) %>% 
  select(-NOX, -date)

m <- nrow(df_explain)

explanation <- map_dfr(
  1:m,
  make_explanation,
  explainer = explainer,
  df = df_explain,
  n_features = 1
)

explanation %>% View

# Explicação geral
explanation %>% 
  filter(feature == "hm") %>%
  mutate(feature_value = as.numeric(feature_value)) %>% 
  ggplot(aes(x = feature_value, y = feature_weight)) +
  geom_point() +
  labs(
    x = "Umidade",
    y = "Coeficiente no modelo simples"
  ) +
  theme_bw()

ggsave(
  filename = "text/figuras/cap-aprend-estat-rf-explanation.pdf",
  width = 6,
  height = 4
)

explanation %>% 
  group_by(feature) %>% 
  summarise(feature_weight = mean(feature_weight))
