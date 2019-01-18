# Libraries ---------------------------------------------------------------

library(tidyverse)

# Data ---------------------------------------------------------------------

df <- read_rds("data/artaxo-salvo-geiger/data-asg-nox.rds")

# EDA ----------------------------------------------------------------------

df %>%
  gather(var, value, o3_mass_conc, NOx, indice) %>% 
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  facet_grid(var~stationname)

df %>% 
  ggplot(aes(x = stationname, y = indice)) + 
  geom_boxplot()

df %>% 
  ggplot(aes(x = indice)) +
  geom_histogram() +
  facet_wrap(~stationname, scales = "free")

df %>% 
  filter(!month %in% 6:9) %>% 
  ggplot(aes(x = share_gas, y = indice)) +
  geom_point() +
  coord_cartesian(ylim = 0:100)
