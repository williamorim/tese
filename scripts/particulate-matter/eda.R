library(tidyverse)

df_pm <- read_rds("data/particulate-matter/pm.rds")

# Series plot -------------------------------------------------------------

# PM2.5

df_pm %>% 
  filter(parameter == "MP2.5") %>%
  ggplot(aes(x = datetime, y = mass_conc)) +
  geom_line(alpha = 0.3) +
  geom_smooth(se = FALSE) +
  facet_wrap(~stationname, scales = "free") +
  theme_bw()

# PM10

df_pm %>% 
  filter(parameter == "MP10") %>%
  ggplot(aes(x = datetime, y = mass_conc)) +
  geom_line(alpha = 0.3) +
  geom_smooth(se = FALSE) +
  facet_wrap(~stationname, scales = "free") +
  theme_bw()
  

# Hourly mean -------------------------------------------------------------

df_pm %>% 
  group_by(parameter, hour, dayofweek) %>% 
  summarise(mass_conc = mean(mass_conc, na.rm = TRUE)) %>%
  mutate(
    dayofweek = forcats::lvls_reorder(dayofweek, c(2, 6, 7, 5, 1, 3, 4))
  ) %>% 
  ggplot(aes(x = hour, y = mass_conc)) +
  geom_line() +
  facet_grid(parameter ~ dayofweek) +
  scale_x_continuous(breaks = seq(0, 24, 4)) +
  theme_bw()


# Yearly mean -------------------------------------------------------------

# MP2.5

df_pm %>%
  filter(hour %in% 6:22, parameter == "MP2.5") %>% 
  mutate(year = lubridate::year(date)) %>% 
  group_by(year, parameter, stationname) %>% 
  summarise(mass_conc = mean(mass_conc, na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = mass_conc)) +
  geom_line() +
  facet_wrap(~stationname) +
  scale_x_continuous(breaks = seq(2008, 2017, 2))

# MP10

df_pm %>%
  filter(hour %in% 6:22, parameter == "MP10") %>% 
  mutate(year = lubridate::year(date)) %>% 
  group_by(year, parameter, stationname) %>% 
  summarise(mass_conc = mean(mass_conc, na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = mass_conc)) +
  geom_line() +
  facet_wrap(~stationname) +
  scale_x_continuous(breaks = seq(2008, 2017, 2))

#MP10

df_pm %>%
  filter(hour %in% 6:22, parameter == "MP10", !dayofweek %in% c("sat", "sun")) %>% 
  mutate(year = lubridate::year(date)) %>% 
  group_by(year, stationname) %>% 
  summarise(mass_conc = mean(mass_conc, na.rm = TRUE)) %>%
  ungroup() %>% 
  #mutate(stationname = forcats::fct_reorder(stationname, mass_conc, na.rm = TRUE)) %>% 
  ggplot(aes(x = stationname, y = mass_conc, fill = stationname)) +
  geom_bar(stat = "identity") +
  facet_wrap(~year) +
  theme(
    legend.position = "top", 
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

