# Tese de Doutorado
# Estratégias para análise de dados de poluição do ar
# Maio de 2018
# William Nilson de Amorim

library(tidyverse)
library(patchwork)
set.seed(7)


# Exemplo trade-off viés/variancia ----------------------------------------

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
  ggtitle("(a)")

p2 <- ggplot(dados, aes(x = x, y = y)) + geom_point() + 
  geom_smooth(formula = y ~ x, colour = "red", se = FALSE, method = 'lm') +
  geom_smooth(formula = y ~ poly(x, 2), colour = "orange", se = FALSE, method = 'lm') +
  geom_smooth(formula = y ~ poly(x, 9), colour = "blue", se = FALSE, method = 'lm') +
  theme_bw() +
  ggtitle("(b)")

p3 <- ggplot(dados, aes(x = x, y = y)) + geom_point() + 
  geom_smooth(formula = y ~ x, colour = "red", se = FALSE, method = 'lm') +
  geom_smooth(formula = y ~ poly(x, 2), colour = "orange", se = FALSE, method = 'lm') +
  geom_smooth(formula = y ~ poly(x, 9), colour = "blue", se = FALSE, method = 'lm') +
  geom_point(data = dados2, aes(x = x, y = y)) +
  theme_bw() +
  ggtitle("(c)")

p4 <- ggplot(dados2, aes(x = x, y = y)) + geom_point() + 
  geom_smooth(formula = y ~ x, colour = "red", se = FALSE, method = 'lm') +
  geom_smooth(formula = y ~ poly(x, 2), colour = "orange", se = FALSE, method = 'lm') +
  geom_smooth(formula = y ~ poly(x, 9), colour = "blue", se = FALSE, method = 'lm') +
  theme_bw()  +
  ggtitle("(d)")

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

rmse1 <-
  map(1:9, poly_lm, dados = dados) %>% 
  map(calcula_rmse) %>% 
  purrr::flatten_dbl()

rmse2 <-
  map(1:9, poly_lm, dados = dados2) %>% 
  map(calcula_rmse) %>% 
  purrr::flatten_dbl()

tibble(
  `Grau do polinômio` = 1:9, 
  RMSE = rmse1, 
  RMSE2 = rmse2,
  Placeholder = ""
) %>% 
  knitr::kable(format = "latex", align = c("c", "c", "c"))

tibble(`Grau do polinômio` = 1:9, RMSE = rmse) %>% 
  knitr::kable(format = "html", align = c("c", "c"))





