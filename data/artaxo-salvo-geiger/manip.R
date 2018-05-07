# Building Artaxo-Salvo-Geiger dataset from original data

# Libraries ---------------------------------------------------------------

library(readr)
library(magrittr)
library(dplyr)
library(forcats)
library(stringr)
library(lubridate)


# Transformations ---------------------------------------------------------

df <- read_rds("data/artaxo-salvo-geiger/dados_originais.rds")

# Cleaning the dataset for the ozone analysis

o3_stations <- c(1, 2, 3, 5, 6, 7, 15, 18, 22, 27, 29, 31)

df <- df %>% 
  select(date, year, month, week, day, dayofweek, hour, 
         dv_publicholiday, dv_weekday_regular, dv_yearendvacation,
         siteid, 
         o3_mass_conc, share_gas, share_gas_aggr,
         rd, tp, hm, ws, pp, ends_with("9am"),
         starts_with("congestion"), dv_beltway_open) %>% 
  mutate(
    dv_o3 = ifelse(siteid %in% o3_stations, 1, 0),
    date = str_c(year, month, day, sep = "-") %>% ymd(),
    stationname = case_when(
      siteid == 1 ~ "Dom Pedro II",
      siteid == 2 ~ "Santana",
      siteid == 3 ~ "Mooca",
      siteid == 5 ~ "Ibirapuera",
      siteid == 6 ~ "Nossa Senhora do O",
      siteid == 7 ~ "Sao Caetano do Sul",
      siteid == 8 ~ "Congonhas",
      siteid == 10 ~ "Cerqueira Cesa",
      siteid == 15 ~ "Diadema",
      siteid == 18 ~ "Santo Andre 1",
      siteid == 22 ~ "Maua",
      siteid == 27 ~ "Pinheiros",
      siteid == 29 ~ "Parelheiros",
      siteid == 31 ~ "IPEN"
    )
  )

morning_vars <- df %>%
  filter(hour %in% 7:11) %>% 
  select(date, stationname, starts_with("congestion")) %>% 
  group_by(date, stationname) %>% 
  summarise_all(.funs = funs(mean), na.rm = TRUE) %>% 
  ungroup()

afternoon_vars <- df %>% 
  filter(hour %in% 12:16) %>% 
  select(date, stationname, o3_mass_conc, share_gas, rd:ws) %>% 
  group_by(date, stationname) %>% 
  summarise_all(.funs = funs(mean), na.rm = TRUE) %>% 
  ungroup()

afternoon_pp <- df %>% 
  filter(hour %in% 12:16) %>% 
  group_by(date, stationname) %>% 
  summarise(pp = mean(pp, na.rm = TRUE)*n()) %>% 
  ungroup()

other_vars <- df %>% 
  select(date, stationname, year:dayofweek, starts_with("dv_")) %>% 
  group_by(date, stationname) %>% 
  summarise_all(.funs = funs(first)) %>% 
  ungroup()

df <- inner_join(morning_vars, afternoon_vars, by = c("date", "stationname")) %>% 
  inner_join(other_vars, by = c("date", "stationname")) %>% 
  inner_join(afternoon_pp, by = c("date", "stationname"))


# Validation

df_salvo <- df %>% 
  filter(!month %in% 6:9) %>% 
  na.omit()

nrow(df_salvo) == 13203 # Number of observations
round(mean(df_salvo$o3_mass_conc), 1) == 72.2 # Mean ozone concentration

# Saving

write_rds(
  df,
  path = "data/artaxo-salvo-geiger/data-asg.rds", 
  compress = "gz" 
)
