# Tese de Doutorado
# Estratégias para análise de dados de poluição do ar
# Abril de 2017
# William Nilson de Amorim

library(tidyverse)

set.seed("5893524")


# Local regression --------------------------------------------------------

data <- tibble(x = rnorm(300, 1, 2),
               y = 1 - 2*x - 3*x^2 + 1*x^3 + 20*rnorm(300)) %>%
  arrange(x) %>% 
  mutate(reg = c(rep(1, 55), rep(0, 30), rep(1, 215)),
         point = c(rep(1, 69), 0, rep(1, 230)))
      

data %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point(aes(color = as.factor(reg))) +
  geom_smooth(se = F, method = "loess") +
  scale_color_manual(values = c("red", "black"))

# inverse-normal-distribution ------------------------------------------------------

f <- function(x, mu, phi) {
  
  sqrt(phi / (2 * pi * x^3)) * exp(-((phi * (x - mu)^2) / (2 * mu^2 * x)))
  
}

df <- data_frame(
  x = seq(0.01, 3, 0.01)
)

df %>% 
  expand(x, phi = seq(1, 30, 2)) %>% 
  mutate(fx = map2(x, phi, f, mu = 1)) %>% 
  arrange(desc(phi)) %>% 
  unnest(fx) %>% 
  ggplot(aes(x = x, y = fx, color = phi, group = phi)) +
  geom_line(alpha = 0.6) +
  labs(y = "Função densidade", x = "y", color = expression(phi)) +
  theme_bw()
ggsave(filename = "figuras/inverse-normal-distribution.pdf")

# gamma-distribution ------------------------------------------------------

f <- function(x, mu, phi) {
  
  (1 / gamma(phi)) * ((phi * x) / mu)^phi * exp(-((phi * x) / mu)) * (1/x)
  
}

df <- data_frame(
  x = seq(0.01, 4, 0.01)
)

df %>% 
  expand(x, phi = seq(1, 20, 2)) %>% 
  mutate(fx = map2(x, phi, f, mu = 1)) %>% 
  unnest(fx) %>% 
  ggplot(aes(x = x, y = fx, color = phi, group = phi)) +
  geom_line() +
  labs(y = "Função densidade", x = "y", color = expression(phi)) +
  theme_bw()
ggsave(filename = "figuras/gamma-distribution.pdf")

# exemplo-serie-tendencia-linear-quadratica -------------------------------

tibble(x = 1:1000, 
       Linear = x/500 + rnorm(1000), 
       Quadrática = x^2/200000 + rnorm(1000)) %>%
  gather(key = "serie", value = "valor", Linear, `Quadrática`) %>% 
  ggplot(aes(x = x, y = valor)) +
  geom_line(size = 0.5) +
  geom_smooth(se = FALSE) +
  facet_grid(. ~ serie) + 
  labs(x = "Instante", y = "Variável de interesse") +
  theme_bw() 
ggsave(filename = "exemplo-serie-tendencia-linear-quadratica.pdf")


# exemplo-serie-tendencias-diferentes -------------------------------------

tibble(x = 1:1000, 
       y = c(rnorm(500), x[1:500]/100 + rnorm(500))) %>%
  ggplot(aes(x = x, y = y)) +
  geom_line(size = 0.5) +
  geom_smooth(se = FALSE) +
  labs(x = "Instante", y = "Variável de interesse") +
  theme_bw() 
ggsave(filename = "figuras/exemplo-serie-tendencias-diferentes.pdf")


# exemplo-residuos-linearidade-forma-U ------------------------------------

data("Auto", package = "ISLR")

fit <- lm(mpg ~ horsepower, data = Auto)

residuos <- fit %>% resid
pred <- fit$fitted.values

qplot(x = pred, y = residuos, geom = "point") +
  labs(x = "Valores preditos", y = "Resíduos") +
  theme_bw()
ggsave(filename = "figuras/exemplo-residuos-linearidade-forma-U.pdf")


# exemplo-serie-correlacao -----------------------------------------------

a <- 0.9
b <- 4

X <- rpois(500, 2)
S <- b*X[1] + rnorm(1)
n <- 500


for(i in 2:n) {
  
  S[i] <- a*S[i-1] + b*X[i] + rnorm(1)
  
}

fit <- lm(S ~ X)

S2 <- b*X + rnorm(n)

fit2 <- lm(S2 ~ X)

tibble(`Dados autocorrelacionados` = fit$residuals,
       `Dados não correlacionados` = fit2$residuals,
       time = 1:n) %>%
  filter(time > 10) %>% 
  gather(key = "modelo", value = "residuos", -time) %>% 
  ggplot(aes(x = time, y = residuos)) +
  geom_point() +
  geom_line(size = 0.5) +
  geom_hline(yintercept = 0, linetype = 2, color = "grey") +
  facet_wrap(~modelo, scales = "free_y") +
  labs(x = "Instante", y = "Resíduos") +
  theme_bw() 
  ggsave(filename = "text/figuras/exemplo-serie-correlacao.pdf", width = 6, height = 4)



# exemplo-serie-heteroscedasticidade --------------------------------------

fit <- tibble(x = rgamma(n = 1000, shape = 1, rate = 2), 
              y = 2*x + rnorm(n = 1000, mean = 0, sd = {1+(1:1000/100)})) %>%
  lm(y ~ x, data = .)

tibble(res = fit$residuals,
       pred = fit$fitted.values) %>% 
  ggplot(aes(x = pred, y = res)) +
  geom_point(size = 0.5) +
  labs(x = "Valores preditos", y = "Resíduos") +
  theme_bw() 
ggsave(filename = "exemplo-serie-heteroscedasticidade.pdf")


# suposicao-linearidade ---------------------------------------------------

tibble(x = 1:10, 
       y = seq(10, 100, 10)) %>%
  ggplot(aes(x = x, y = y)) +
  geom_line(size = 0.5) +
  geom_segment(aes(x = 0, xend = 3, y = 30, yend = 30), linetype = 4,
               color = "light blue") +
  geom_segment(aes(x = 3, xend = 3, y = 0, yend = 30), linetype = 4,
               color = "light blue") +
  geom_segment(aes(x = 0, xend = 4, y = 40, yend = 40), linetype = 4,
               color = "light blue") +
  geom_segment(aes(x = 4, xend = 4, y = 0, yend = 40), linetype = 4,
               color = "light blue") +
  geom_segment(aes(x = 0, xend = 7, y = 70, yend = 70), linetype = 3,
               color = "light green") +
  geom_segment(aes(x = 7, xend = 7, y = 0, yend = 70), linetype = 3,
               color = "light green") +
  geom_segment(aes(x = 0, xend = 8, y = 80, yend = 80), linetype = 3,
               color = "light green") +
  geom_segment(aes(x = 8, xend = 8, y = 0, yend = 80), linetype = 3,
               color = "light green") +
  geom_segment(aes(x = 3, xend = 3, y = 30, yend = 40),
               arrow = arrow(ends = "both", length = unit(0.02, "npc"))) +
  geom_segment(aes(x = 7, xend = 7, y = 70, yend = 80),
               arrow = arrow(ends = "both", length = unit(0.02, "npc"))) +
  coord_cartesian(xlim = c(1, 10), ylim = c(10, 100)) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(breaks = seq(10, 100, 10)) +
  annotate("text", x = 2.5, y = 35, label = expression(hat(beta))) +
  annotate("text", x = 6.5, y = 75, label = expression(hat(beta))) +
  labs(x = "X", y = expression(hat(Y))) +
  theme_bw() +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank())
ggsave(filename = "figuras/suposicao-linearidade.pdf")


# --------------------------------------------------------------------------

n <- 200
q <- 100
b0 <- 1
b1 <- 2
a <- 3

dados <- tibble(
  id = factor(rep(1:q, rep(n/q, q))),
  x = rpois(n, lambda = 2),
  z = rep(rnorm(q, 0, a), rep(n/q, q)),
  e = rnorm(n),
  y = b0 + b1*x + z + e
)

dados %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point()

dados %>% 
  group_by(x) %>% 
  summarise(var = var(y), mean = mean(y)) %>% 
  mutate(true_mean = b0 + b1*x)

fit <- lm(y ~ x + as.factor(id), dados)
lm(y ~ x, dados) %>% summary
lme4::lmer(y ~ x + (1|id), dados) %>% summary()

coef <- fit$coefficients %>% as.numeric()
coef2 <- coef[-c(1,2)]
mean(coef[1] + coef2)
