# Libraries ---------------------------------------------------------------

library(tidyverse)

# Data ---------------------------------------------------------------------

df <- read_rds("data/artaxo-salvo-geiger/data-asg-nox.rds")

# EDA ----------------------------------------------------------------------

df %>%
  gather(var, value, o3_mass_conc, NOx, indice) %>% 
  mutate(
    var = case_when(
      var == "o3_mass_conc" ~ "O3",
      var == "indice" ~ "Índice",
      is.na(var) ~ NA_character_,
      TRUE ~ "NOx"
    )
  ) %>% 
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  facet_grid(var~stationname, scales = "free") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(x = "Ano", y = expression(paste("Índice/Cocentração", " (", mu, "g/", m^3, ")"))) +
  theme_bw()

df %>% 
  ggplot(aes(x = date, y = indice)) +
  geom_line() +
  facet_wrap(~stationname, scales = "free") +
  scale_x_date(date_breaks = "1 years", date_labels = "%Y") +
  labs(x = "Ano", y = "Índice") +
  theme_bw()
ggsave(
  "text/figuras/cap-mort-indice-eda.pdf",
  width = 10,
  height = 6
)

df %>% 
  select(indice, stationname) %>% 
  group_by(stationname) %>% 
  skimr::skim_to_wide() %>% 
  select(-type, -(variable:n), -hist) %>% 
  select(
    `Estação` = stationname, `Média` = mean, `DP` = sd, Min = p0, 
    Q1 = p25, Q2 = p50, Q3 = p75, Max = p100
  ) %>% 
  mutate(pc = "") %>% 
  knitr::kable(format = "latex")

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
