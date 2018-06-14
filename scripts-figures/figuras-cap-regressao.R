# Tese de Doutorado
# Estratégias para análise de dados de poluição do ar
# Junho de 2018
# William Nilson de Amorim

library(tidyverse)
set.seed("5893524")

# exemplo-splines

a <- 1
b <- 0.4
c <- - 0.8
d <- - 0.6
e <- 0.3

x <- seq(-1, 3, 0.01)
erro <- rnorm(length(x))
y <- a + b*x + c*x^2 + d*x^3 + e*x^4 + erro

df <- tibble(
  x = x,
  y = y,
  knot = case_when(
    x < -0.5 ~ 1,
    x < 0 ~ 2,
    x < 0.5 ~ 3,
    x < 1 ~ 4,
    TRUE ~ 5
  )
)

ggplot(df, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(
    aes(color = as.factor(knot), group = as.factor(knot)),
    show.legend = FALSE,
    se = FALSE,
    method = "lm",
    formula = "y ~ poly(x, 3)"
  ) +
  theme_bw()
ggsave(filename = "text/figuras/cap-regressao-exemplo-splines.pdf", 
       width = 6, height = 4)
