# Libraries

library(flexdashboard)
library(tidyverse)
library(lubridate)

# Data

df <- read_rds(
  "../../data/cetesb-greve-caminhoneiros/data-greve-caminhoneiros.rds"
)

# Functions

make_series_plot <- function(df, pollutant, year_, hours) {
  
  if(year_ == 2018) {
    p_lab <- labs(x = "Dia", y = pollutant)
  } else {
    p_lab <- labs(x = "", y = pollutant)
  }
  
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
    facet_grid(year ~ stationname, scales = "free_y") +
    theme_bw() +
    scale_x_date(
      labels = scales::date_format("%d-%m"),
      breaks = c(
        lubridate::dmy(paste0("01-05-", year_)), 
        lubridate::dmy(paste0("01-06-", year_))
      ),
      date_breaks = "1 month"
    ) +
    p_lab
  
}

make_grid_plot <- function(df, pollutant, hours) {
  
  map(
    2016:2018, 
    make_series_plot,
    df = df,
    pollutant = pollutant,
    hours = hours
  ) %>% 
    patchwork::wrap_plots(nrow = 3) %>% 
    print()
  
}

make_bar_plot <- function(df, pollutant_, hours) {
  
  df %>% 
    mutate(period = case_when(
      date > dmy("09-05-2018") & date < dmy("16-05-2018") ~ 1,
      date > dmy("23-05-2018") & date < dmy("30-05-2018") ~ 2,
      date > dmy("06-06-2018") & date < dmy("14-06-2018") ~ 3,
      TRUE ~ 0
    )) %>%
    filter(period != 0) %>% 
    gather(pollutant, conc, CO:O3) %>% 
    group_by(pollutant, stationname, period) %>%
    filter(hour %in% hours, pollutant == pollutant_) %>% 
    summarise(conc = mean(conc, na.rm = TRUE)) %>%
    ggplot(aes(x = stationname, y = conc, fill = as.factor(period))) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(y = pollutant_, fill = "Período", x = "Estação") +
    scale_fill_discrete(
      labels = c("09/05 a 16/05", "23/05 a 30/05",  "06/06 a 14/06")
    ) +
    theme(legend.position = "bottom")
}
