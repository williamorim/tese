library(tidyverse)

df_pm <- read_rds("data/particulate-matter/pm.rds")

df_pm <- df_pm %>% 
  mutate(
    hour = hour - 1,
    datetime = paste(date, hour), 
    datetime = lubridate::ymd_h(datetime)
  )

# Series plot -------------------------------------------------------------

# PM2.5

df_pm %>% 
  filter(parameter == "MP2.5") %>%
  ggplot(aes(x = datetime, y = mass_conc)) +
  geom_line(alpha = 0.3) +
  geom_smooth(se = FALSE) +
  facet_wrap(~stationname, scales = "free")

# PM10

df_pm %>% 
  filter(parameter == "MP10") %>%
  ggplot(aes(x = datetime, y = mass_conc)) +
  geom_line(alpha = 0.3) +
  geom_smooth(se = FALSE) +
  facet_wrap(~stationname, scales = "free")
  

# Hourly mean -------------------------------------------------------------

df_pm %>% 
  group_by(parameter, hour, dayofweek) %>% 
  summarise(mass_conc = mean(mass_conc, na.rm = TRUE)) %>%
  mutate(
    dayofweek = forcats::lvls_reorder(dayofweek, c(2, 6, 7, 5, 1, 3, 4))
  ) %>% 
  ggplot(aes(x = hour, y = mass_conc)) +
  geom_line() +
  facet_grid(parameter ~ dayofweek)


