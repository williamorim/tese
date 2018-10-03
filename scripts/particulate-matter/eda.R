library(tidyverse)
library(plotly)

df_pm <- read_rds("data/particulate-matter/pm.rds") %>% 
  filter(valido == "Sim")

# Series plot -------------------------------------------------------------

# PM2.5

df_pm %>% 
  filter(parameter == "MP2.5") %>%
  ggplot(aes(x = date_time, y = mass_conc)) +
  geom_line(alpha = 0.3) +
  geom_smooth(se = FALSE) +
  facet_wrap(~stationname, scales = "free") +
  theme_bw() +
  labs(x = "Ano", y = expression(paste("MP2.5 (", mu, "g/", m^3, ")")))

# PM10

df_pm %>% 
  filter(parameter == "MP10") %>%
  ggplot(aes(x = date_time, y = mass_conc)) +
  geom_line(alpha = 0.3) +
  geom_smooth(se = FALSE) +
  facet_wrap(~stationname, scales = "free") +
  theme_bw() +
  labs(x = "Ano", y = expression(paste("MP10 (", mu, "g/", m^3, ")")))
  

# Hourly mean -------------------------------------------------------------

df_pm %>% 
  mutate(dayofweek = lubridate::wday(date, label = TRUE)) %>%
  group_by(parameter, hour, dayofweek) %>% 
  summarise(mass_conc = mean(mass_conc, na.rm = TRUE)) %>% 
  ggplot(aes(x = hour, y = mass_conc)) +
  geom_line() +
  facet_grid(parameter ~ dayofweek) +
  scale_x_continuous(breaks = seq(0, 24, 4)) +
  theme_bw() +
  labs(x = "Hora", y = expression(paste("MP (", mu, "g/", m^3, ")")))


# Yearly mean -------------------------------------------------------------

# MP2.5

df_pm %>%
  filter(
    hour %in% 6:22, 
    parameter == "MP2.5",
    !stationname %in% c("Osasco", "Santana")
  ) %>% 
  mutate(year = lubridate::year(date)) %>% 
  group_by(year, parameter, stationname) %>% 
  summarise(mass_conc = mean(mass_conc, na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = mass_conc)) +
  geom_line() +
  facet_wrap(~stationname) +
  scale_x_continuous(breaks = seq(2008, 2017, 2)) +
  labs(x = "Hora", y = expression(paste("MP2.5 (", mu, "g/", m^3, ")")))

# MP10

df_pm %>%
  filter(hour %in% 6:22, parameter == "MP10") %>% 
  mutate(year = lubridate::year(date)) %>% 
  group_by(year, parameter, stationname) %>% 
  summarise(mass_conc = mean(mass_conc, na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = mass_conc)) +
  geom_line() +
  facet_wrap(~stationname) +
  scale_x_continuous(breaks = seq(2008, 2017, 2)) +
  labs(x = "Hora", y = expression(paste("MP10 (", mu, "g/", m^3, ")")))

#MP10

p <- df_pm %>%
  filter(hour %in% 6:22, parameter == "MP10", !dayofweek %in% c("sat", "sun")) %>% 
  mutate(year = lubridate::year(date)) %>% 
  group_by(year, stationname) %>% 
  summarise(mass_conc = mean(mass_conc, na.rm = TRUE)) %>%
  ungroup %>% 
  rename(Station = stationname, Year = year, Mass = mass_conc) %>%
  mutate(Mass = round(Mass, 2)) %>% 
  ggplot(aes(x = Year, y = Mass, color = Station)) +
  geom_line() +
  geom_point() +
  theme(
    legend.position = "top"
  ) +
  labs(
    x = "Year",
    y = "PM10",
    color = ""
  ) +
  scale_x_continuous(breaks = seq(2008, 2017, 3)) +
  theme_bw()

plotly::ggplotly(p)
