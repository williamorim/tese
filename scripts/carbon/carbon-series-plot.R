
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(leaflet)
library(ggridges)


# Data --------------------------------------------------------------------

# Código para baixar e consolidar os dados: `data/cestb-carbon/manip.R`

df_carbon <- read_rds("data/cetesb-carbon/data-carbon-2008-2018.rds")
station_coord <- readxl::read_excel("data/cetesb_station_geoposition.xlsx")

# Série horária -----------------------------------------------------------

df_carbon %>%
  filter(stationname == "Pinheiros") %>% 
  group_by(dayofweek, hour) %>% 
  summarise(co_mass_conc = mean(co_mass_conc)) %>% 
  ggplot(aes(x = hour, y = co_mass_conc)) +
  geom_line() +
  facet_wrap(~dayofweek, nrow = 2) +
  theme_bw() +
  labs(x = "Hora", y = "CO (ppm)") +
  scale_x_continuous(breaks = c(1, 6, 11, 16, 21, 24))


# Série da média diária pela manhã ----------------------------------------

# Filtrando para a estação de Pinheiros, das 7 às 11h da manhã, 
# apenas para dias da semana.

df_carbon %>%
  filter(
    stationname == "Pinheiros", 
    !dayofweek %in% c("sáb","dom"),
    hour %in% 7:11
  ) %>%
  group_by(date) %>% 
  summarise(co_mass_conc = mean(co_mass_conc)) %>% 
  ggplot(aes(x = date, y = co_mass_conc)) +
  geom_line() +
  geom_smooth() +
  theme_bw() +
  labs(x = "Ano", y = "CO (ppm)")

# Uber chegou em São Paulo em 26/06/2014.
# Sergio falou que é devido ao padrão da emissão veicular.

# Distribuição mensal do monóxido de carbono ------------------------------

df_carbon %>%
  filter(
    stationname == "Pinheiros",
    !dayofweek %in% c("sáb","dom"),
    hour %in% 7:11
  ) %>%
  group_by(date) %>% 
  summarise(co_mass_conc = mean(co_mass_conc, na.rm = TRUE)) %>%
  mutate(month = lubridate::month(date, label = TRUE),
         month = forcats::fct_rev(as.factor(month)),
         co_mass_conc = co_mass_conc + 1) %>% 
  ggplot(aes(x = co_mass_conc, y = month)) +
  geom_density_ridges(aes(fill = month), show.legend = FALSE) +
  xlim(0.3, 5) +
  theme_bw() +
  labs(x = "CO (ppm)", y = "Mês")


# Outras estações ---------------------------------------------------------

# Mapa das estações
df_carbon %>%
  left_join(station_coord, by = "stationname") %>% 
  distinct(stationname, .keep_all = TRUE) %>% 
  leaflet() %>%
  addTiles() %>% 
  addMarkers(lng = ~long, lat = ~lat, popup = ~stationname)

# Série das outras estações
df_carbon %>%
  filter(
    !stationname %in% c("IPEN-USP", "Pinheiros"),
    !dayofweek %in% c("sáb","dom"),
    hour %in% 7:11
  ) %>%
  group_by(stationname, date) %>% 
  summarise(co_mass_conc = mean(co_mass_conc)) %>%
  filter(co_mass_conc < 10, co_mass_conc > 0) %>% 
  ggplot(aes(x = date, y = co_mass_conc)) +
  geom_line() +
  geom_smooth() +
  facet_wrap(~stationname, scales = "free_y", nrow = 3) +
  theme_bw() +
  labs(x = "Ano", y = "CO (ppm)") +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y")

# Distribuição das estações

df_carbon %>%
  filter(
    !dayofweek %in% c("sáb","dom"),
    hour %in% 7:11
  ) %>%
  mutate(
    co_mass_conc = co_mass_conc + 1,
    stationname = forcats::fct_rev(as.factor(stationname))
  ) %>%
  ggplot(aes(x = co_mass_conc, y = stationname)) +
  geom_density_ridges(aes(fill = stationname), show.legend = FALSE) +
  xlim(0.8, 5.5) +
  theme_bw() +
  labs(x = "CO (ppm)", y = "Mês")

# Gráfico de barra das médias anuais
df_carbon %>%
  filter(
    !dayofweek %in% c("sáb","dom"),
    hour %in% 7:11
  ) %>%
  mutate(year = lubridate::year(date)) %>% 
  group_by(stationname, year) %>% 
  summarise(co_mass_conc = mean(co_mass_conc, na.rm = TRUE)) %>%
  ggplot(aes(x = stationname, y = co_mass_conc)) +
  geom_bar(aes(fill = stationname), stat = "identity") +
  facet_wrap(~year) +
  theme_bw() +
  labs(y = "CO (ppm)", fill = "Estações") +
  scale_x_discrete(breaks = element_blank()) +
  theme(legend.position = "bottom", axis.title.x = element_blank())

# Médias por mês
df_carbon %>%
  filter(
    !dayofweek %in% c("sáb","dom"),
    hour %in% 7:11
  ) %>%
  mutate(year = lubridate::year(date), month = lubridate::month(date)) %>% 
  group_by(stationname, year, month) %>% 
  summarise(co_mass_conc = mean(co_mass_conc, na.rm = TRUE)) %>%
  mutate(date = lubridate::ymd(str_c(year, month, "01", sep = "-"))) %>% 
  ggplot(aes(x = date, y = co_mass_conc)) +
    geom_line(aes(colour = stationname))

# Mapa de calor
df_carbon %>%
  filter(
    !dayofweek %in% c("sáb","dom"),
    hour %in% 7:11,
    lubridate::year(date) == 2018
  ) %>%
  group_by(stationname) %>% 
  summarise(co_mass_conc = mean(co_mass_conc, na.rm = TRUE)) %>% 
  left_join(station_coord, by = "stationname") %>% 
  leaflet() %>%1354
  addProviderTiles(providers$CartoDB.Positron) %>%
  addHeatmap(
    lng = ~long, 
    lat = ~lat, 
    intensity = ~co_mass_conc,
    blur = 30,
    radius = 30
  )


