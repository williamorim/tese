# Tese de Doutorado
# Estratégias para análise de dados de poluição do ar
# Junho de 2018
# William Nilson de Amorim

library(tidyverse)
library(patchwork)
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

# exemplo regressão segmentada

df <- readr::read_rds("data/artaxo-salvo-geiger/data-asg-model.rds")

fit <- lm(o3_mass_conc ~ tp, data = df)
fit_seg <- segmented::segmented(fit,seg.Z = ~tp, psi = 20)


p1 <- df %>% 
  filter(siteid == 1) %>% 
  ggplot(aes(x = tp, y = o3_mass_conc)) +
  geom_point() +
  theme_bw() +
  labs(
    x = "Temperatura (°C)", 
    y = expression(paste(O[3], " (", mu, "g/", m^3, ")"))
  )

p15 <- df %>% 
  filter(siteid == 1) %>% 
  ggplot(aes(x = tp, y = o3_mass_conc)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  labs(
    x = "Temperatura (°C)", 
    y = expression(paste(O[3], " (", mu, "g/", m^3, ")"))
  )

p2 <- df %>% 
  filter(siteid == 1) %>% 
  ggplot(aes(x = tp, y = o3_mass_conc)) +
  geom_point() +
  geom_segment(x = 0, y = -2.069, xend = 21.66, yend = 38.5548, color = "blue") +
  geom_segment(xend = 38, yend = 151.9, x = 21.66, y = 38.5548, color = "orange") +
  theme_bw() +
  labs(
    x = "Temperatura (°C)", 
    y = expression(paste(O[3], " (", mu, "g/", m^3, ")"))
  )

p <- p1 + p2

ggsave("text/figuras/cap-regressao-exemplo-reg-seg.pdf", p, width = 6, height = 4)
