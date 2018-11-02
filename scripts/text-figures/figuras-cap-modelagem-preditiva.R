# Tese de Doutorado
# Estratégias para análise de dados de poluição do ar
# Maio de 2018
# William Nilson de Amorim


# Exemplo trade-off viés/variancia ----------------------------------------

library(tidyverse)
library(patchwork)

set.seed(7)

dados <- data_frame(
  x = runif(10),
  y = 2*x + 3*x^2 + rnorm(10, 0, 0.15) 
)

dados2 <- data_frame(
  x = runif(100),
  y = 2*x + 3*x^2 + rnorm(100, 0, 0.1) 
)

modelo <- lm(y ~ x, data = dados)
modelo2 <- lm(y ~ poly(x, 2), data = dados)


p1 <- ggplot(dados, aes(x = x, y = y)) + 
  geom_point() + 
  theme_bw() +
  ggtitle("(a)") +
  scale_y_continuous(limits = c(-4, 12), breaks = c(0, 4, 8))

p2 <- ggplot(dados, aes(x = x, y = y)) + geom_point() + 
  geom_smooth(formula = y ~ x, colour = "red", se = FALSE, method = 'lm') +
  geom_smooth(formula = y ~ poly(x, 2), colour = "orange", se = FALSE, method = 'lm') +
  geom_smooth(formula = y ~ poly(x, 9), colour = "blue", se = FALSE, method = 'lm') +
  theme_bw() +
  ggtitle("(b)") +
  scale_y_continuous(limits = c(-4, 12), breaks = c(0, 4, 8))

p3 <- ggplot(dados, aes(x = x, y = y)) + geom_point() + 
  geom_smooth(formula = y ~ x, colour = "red", se = FALSE, method = 'lm') +
  geom_smooth(formula = y ~ poly(x, 2), colour = "orange", se = FALSE, method = 'lm') +
  geom_smooth(formula = y ~ poly(x, 9), colour = "blue", se = FALSE, method = 'lm') +
  geom_point(data = dados2, aes(x = x, y = y)) +
  theme_bw() +
  ggtitle("(c)") +
  scale_y_continuous(limits = c(-4, 12), breaks = c(0, 4, 8))

p4 <- ggplot(dados2, aes(x = x, y = y)) + geom_point() + 
  geom_smooth(formula = y ~ x, colour = "red", se = FALSE, method = 'lm') +
  geom_smooth(formula = y ~ poly(x, 2), colour = "orange", se = FALSE, method = 'lm') +
  geom_smooth(formula = y ~ poly(x, 9), colour = "blue", se = FALSE, method = 'lm') +
  theme_bw()  +
  ggtitle("(d)") +
  scale_y_continuous(limits = c(-4, 12), breaks = c(0, 4, 8))

p <- p1 + p2 + p3 + p4

ggsave(
  filename = "text/figuras/cap-aprend-estat-trade-off.pdf", 
  plot = p,
  width = 6, height = 4
)

# Tabelas de RSME

poly_lm <- function(dados, d) {
  lm(y ~ poly(x, degree = d, raw = TRUE), data = dados)
}

calcula_rmse <- function(modelo) {
  mean(residuals(modelo)^2) %>%
    sqrt() %>% 
    round(3)
}

calcula_rmse2 <- function(modelo, dados) {
  mean((predict(modelo, newdata = dados) - dados$y)^2) %>% 
    sqrt() %>% 
    round(3)
}

rmse1 <-
  map(1:9, poly_lm, dados = dados) %>% 
  map(calcula_rmse) %>% 
  purrr::flatten_dbl()

rmse2 <-
  map(1:9, poly_lm, dados = dados) %>% 
  map(calcula_rmse2, dados = dados2) %>% 
  purrr::flatten_dbl()

tibble(
  `Grau do polinômio` = 1:9, 
  RMSE = rmse1, 
  RMSE2 = rmse2,
  Placeholder = ""
) %>% 
  knitr::kable(format = "latex", align = c("c", "c", "c"))


# Exemplos árvores de decisão ---------------------------------------------

library(tidyverse)
library(caret)
library(rattle)

dados <- 
  read_rds("data/artaxo-salvo-geiger/data-asg-model.rds") %>%
  filter(stationname == "Dom Pedro II") %>% 
  select(o3_mass_conc, tp) %>% 
  na.omit()
  
train_control <- trainControl(method = "cv", number = 5)

model <- train(
  form = o3_mass_conc ~ tp,
  data = dados,
  method = "rpart",
  trControl = train_control
)

model
fancyRpartPlot(model$finalModel, caption = "")
rattle::savePlotToFile("text/figuras/cap-aprend-estat-arvore.pdf")
