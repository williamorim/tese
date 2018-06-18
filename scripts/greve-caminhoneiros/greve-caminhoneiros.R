
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(leaflet)
library(patchwork)
library(lubridate)

# Data --------------------------------------------------------------------

# Código para baixar e consolidar os dados: `data/cestb-carbon/manip.R`

df <- read_rds("data/cetesb-greve-caminhoneiros/data-greve-caminhoneiros.rds")
station_coord <- readxl::read_excel("data/cetesb_station_geoposition.xlsx")

# Functions ---------------------------------------------------------------

make_series_plot <- function(df, pollutant, year_, hours) {
  
  df %>% 
    filter(
      hour %in% hours
    ) %>% 
    group_by(date, stationname) %>%
    select(y = pollutant, everything()) %>% 
    summarise(conc = mean(y, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(year = lubridate::year(date)) %>%
    filter(year == year_) %>% 
    ggplot(aes(x = date, y = conc)) +
    geom_line() +
    geom_smooth() +
    geom_vline(
      xintercept = lubridate::dmy(paste0("23-05-", year_)),
      linetype = 2,
      color = "red"
    ) +
    geom_vline(
      xintercept = lubridate::dmy(paste0("30-05-", year_)),
      linetype = 2,
      color = "red"
    ) +
    facet_grid(~stationname, scales = "free_y") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
    labs(x = "Dia", y = pollutant)
  
}

make_grid_plot <- function(df, pollutant, hours) {
  
  map(
    2016:2018, 
    make_series_plot,
    df = df,
    pollutant = pollutant,
    hours = hours
  ) %>% 
    wrap_plots(nrow = 3) %>% 
    print()
  
}

# Mapa das estações -------------------------------------------------------

df %>%
  left_join(station_coord, by = "stationname") %>% 
  distinct(stationname, .keep_all = TRUE) %>% 
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(lng = ~long, lat = ~lat, popup = ~stationname)

# Série horária -----------------------------------------------------------

df %>%
  group_by(dayofweek, hour) %>% 
  summarise_at(vars(CO:O3), funs(mean), na.rm = TRUE) %>%
  gather(polluent, conc, CO:O3) %>% 
  ggplot(aes(x = hour, y = conc)) +
  geom_line() +
  facet_grid(polluent ~ dayofweek, scales = "free_y") +
  theme_bw()

# Série CO ----------------------------------------------------------------

# 7h às 11h
df %>% 
  filter(!dayofweek %in% c("sáb","dom")) %>% 
  make_grid_plot("CO", 7:11)

# 18h às 24h
df %>% 
  filter(!dayofweek %in% c("sáb","dom")) %>% 
  make_grid_plot("CO", 18:24)

# Série O3 ----------------------------------------------------------------

# 12h às 17h
df %>%
  filter(!stationname == "Osasco") %>% 
  make_grid_plot("O3", 12:17)


# Série NO ----------------------------------------------------------------

# 7h às 11h
df %>% 
  filter(!dayofweek %in% c("dom")) %>% 
  make_grid_plot("NO", 7:11)


# Série NO2 ---------------------------------------------------------------

# 8h às 20h
df %>% 
  filter(!dayofweek %in% c("sab", "dom")) %>% 
  make_grid_plot("NO2", 8:20)

# PM 2.5 ------------------------------------------------------------------

# ! 10h às 18h
df %>% 
  filter(!dayofweek %in% c("dom")) %>% 
  make_grid_plot("MP2.5", c(1:9, 19:24))


# PM 10 -------------------------------------------------------------------

# 5h às 24h
df %>%
  filter(!stationname == "Ibirapuera") %>% 
  make_grid_plot("MP10", 5:24)


# Médias período anterior -------------------------------------------------

df %>% 
  mutate(period = case_when(
    date > dmy("09-05-2018") & date < dmy("16-05-2018") ~ 1,
    date > dmy("23-05-2018") & date < dmy("30-05-2018") ~ 2,
    TRUE ~ 0
  )) %>%
  filter(period != 0) %>% 
  gather(pollutant, conc, CO:O3) %>% 
  group_by(pollutant, stationname, period) %>% 
  summarise(conc = median(conc, na.rm = TRUE)) %>% View


