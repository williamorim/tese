library(tidyverse)
library(lubridate)

df <- read_rds("data/cetesb-eleicoes/data-eleicoes.rds")

df %>% 
  filter(periodo != "outro") %>% 
  mutate(periodo = forcats::lvls_reorder(periodo, c(1, 3, 4, 2))) %>% 
  group_by(periodo, parameter) %>% 
  summarise(mass_conc = mean(mass_conc, na.rm = TRUE)) %>% 
  ggplot(aes(x = periodo, y = mass_conc)) +
  geom_col() +
  facet_wrap(~parameter, scales = "free")

df %>% 
  filter(parameter == "O3", hour %in% c(12:16)) %>%
  group_by(date, stationname) %>% 
  summarise(mass_conc = mean(mass_conc, na.rm = TRUE)) %>% 
  ggplot(aes(x = date, y = mass_conc)) +
  geom_line() +
  geom_vline(xintercept = as_date("2018-10-07"), linetype = 2, color = "red") +
  geom_vline(xintercept = as_date("2018-10-28"), linetype = 2, color = "red") +
  facet_wrap(~stationname)
