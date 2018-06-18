# Tese de Doutorado
# Estratégias para análise de dados de poluição do ar
# Abril de 2017
# William Nilson de Amorim

library(tidyverse)
library(ggridges)
set.seed("5893524")

# Gráfico de dispersão ----------------------------------------------------

df <- read_rds("scripts/data/df_poluentes_2008_2011.rds")

source("scripts/utils/climate-graphs.R")

# scatter-ozone-no

df %>% 
  filter(stationno == 1, hour %in% 12:16) %>% 
  scatter_plot(yvar = O3, 
               xvars = vars(NO), 
               xlab = expression(paste(NO, " (", mu, "g/", m^3, ") à tarde")), 
               ylab = expression(paste(O[3], " (", mu, "g/", m^3, ") à tarde")))
ggsave(filename = "figuras/cap-analise-explo-scatter-ozone-no.pdf", 
       width = 6, height = 4)


# scatter-ozone-no2

df %>% 
  filter(stationno == 1, hour %in% 12:16) %>% 
  scatter_plot(yvar = O3, 
               xvars = vars(NO2), 
               xlab = expression(paste(NO[2], " (", mu, "g/", m^3, ") à tarde")),
               ylab = expression(paste(O[3], " (", mu, "g/", m^3, ") à tarde")))
ggsave(filename = "figuras/cap-analise-explo-scatter-ozone-no2.pdf", 
       width = 6, height = 4)

# scatter-ozone-no2-morning

ozone <- df %>% 
  filter(stationno == 1, hour %in% 12:16) %>% 
  select(O3) %>% 
  flatten_dbl()

no2 <- df %>% 
  filter(stationno == 1, hour %in% 7:11) %>% 
  select(NO2) %>% 
  flatten_dbl()
  
tibble(O3 = ozone, NO2 = no2) %>% 
  scatter_plot(yvar = O3, 
               xvars = vars(NO2), 
               xlab = expression(paste(NO[2], " (", mu, "g/", m^3, ") de manhã")),
               ylab = expression(paste(O[3], " (", mu, "g/", m^3, ") à tarde")))
ggsave(filename = "figuras/cap-analise-explo-scatter-ozone-no2-morning.pdf", 
       width = 6, height = 4)


# Gráficos e distribuição -------------------------------------------------

# hist-ozone  

df <- readr::read_rds("scripts/data/df_O3_clima_2008_2013.rds")

df %>% 
  filter(siteid == 1, hour %in% 12:16) %>% 
  group_by(date) %>% 
  summarise(o3_mass_conc = mean(o3_mass_conc, na.rm = TRUE)) %>% 
  ggplot(aes(x = o3_mass_conc)) + 
  geom_histogram(fill = "white", color = "black") +
  labs(x = expression(paste(O[3], " (", mu, "g/", m^3, ")")),
       y = "Frequência") +
  theme_bw()
ggsave(filename = "figuras/cap-analise-explo-hist-ozone.pdf", 
       width = 6, height = 4)

# ridges-ozone-month

df %>%
  filter(siteid == 1, hour %in% 12:16) %>% 
  group_by(date) %>% 
  summarise(o3_mass_conc = mean(o3_mass_conc, na.rm = TRUE)) %>% 
  mutate(month = lubridate::month(date),
         month = as.factor(month)) %>% 
  ggplot(aes(x = o3_mass_conc, y = month, fill = month)) + 
  geom_density_ridges(show.legend = FALSE) +
  labs(x = expression(paste(O[3], " (", mu, "g/", m^3, ")")), y = "Mês") +
  theme_bw()
ggsave(filename = "figuras/cap-analise-explo-ridges-ozone-month.pdf", 
       width = 6, height = 4)

  