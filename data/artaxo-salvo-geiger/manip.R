# Building Artaxo-Salvo-Geiger dataset from original data

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)


# Baixando dados de NOx ----------------------------------------------------

library(koffing)

stations <-  c(92, 63, 83, 85, 99, 98, 72, 96, 86, 254, 65, 95)
params <- c(17, 15, 18)

safe_scraper_cetesb <- possibly(
  function(station, parameter, start, end) {
    koffing::scraper_cetesb(
      station = station,
      parameter = parameter,
      start = start,
      end = end,
      login = "thewilliam89@gmail.com",
      password = "wouldy0uralauq?"
    )
  },
  otherwise = NULL
)

get_cetesb_data <- function(station, parameter) {
  
  start <- "01/11/2008"
  end <- "31/05/2013"
  
  df <- safe_scraper_cetesb(station, parameter, start, end)
  
  if(!is.null(df)) {
    
    write_csv(
      x = df, 
      path = str_c(
        "data/artaxo-salvo-geiger/cetesb/df-", 
        parameter, "-", station, ".csv"
      )
    )
    
    print(
      str_c(
        "Dados de ", parameter, " da estação ", 
        station, " baixados com sucesso."
      )
    )
  } else {
    print(
      str_c(
        "Não foi possível baixar dados de ", parameter, " da estação ", 
        station
      )
    )
  }
  
}

for(j in 3) {
  walk(
    stations[7:12], 
    get_cetesb_data,
    params[j]
  )
}

files <- list.files(
  path = "data/artaxo-salvo-geiger/cetesb/",
  full.names = TRUE
)
files <- files[str_detect(files, ".csv")]
files <- files[file.size(files) > 2000]

df_nox <- map_dfr(
  files, 
  read_csv,
  col_types = str_c(rep("c", 9), collapse = "")
)

df_nox <- df_nox %>% 
  select(-mass_conc_movel, -date_time, -dayofweek, -valido) %>%
  mutate(date = ymd(date)) %>%
  mutate_at(.vars = vars(hour, mass_conc), .funs = funs(as.numeric)) %>% 
  mutate(
    stationname = case_when(
      stationname == "Cid.Universitária-USP-Ipen" ~ "IPEN",
      stationname == "Grajaú-Parelheiros" ~ "Parelheiros",
      stationname == "Mauá" ~ "Maua",
      stationname == "Parque D.Pedro II" ~ "Dom Pedro II",
      stationname == "São Caetano do Sul" ~ "Sao Caetano do Sul",
      TRUE ~ stationname
    )
  ) %>% 
  spread(parameter, mass_conc)

write_rds(df_nox, path = "data/artaxo-salvo-geiger/data-nox.rds")

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
  summarise(pp = mean(pp, na.rm = TRUE)) %>%
  ungroup()

other_vars <- df %>% 
  select(
    date, 
    stationname,
    siteid,
    year:dayofweek, 
    starts_with("dv_")
  ) %>% 
  group_by(date, stationname) %>% 
  summarise_all(.funs = funs(first)) %>% 
  ungroup()


df_salvo <- 
  inner_join(morning_vars, afternoon_vars, by = c("date", "stationname")) %>% 
  inner_join(other_vars, by = c("date", "stationname")) %>% 
  inner_join(afternoon_pp, by = c("date", "stationname"))

df_nox <- read_rds("data/artaxo-salvo-geiger/data-nox.rds")

df_salvo_nox <- df_nox %>%
  filter(hour %in% 5:11, NOx < 300, NOx > 0) %>%
  select(date, stationname, NOx) %>% 
  group_by(date, stationname) %>% 
  summarise_all(.funs = funs(mean), na.rm = TRUE) %>% 
  ungroup()  %>% 
  right_join(df_salvo, by = c("stationname", "date")) %>%
  filter(!is.na(o3_mass_conc), !is.na(NOx), o3_mass_conc > 0.1) %>% 
  mutate(indice = NOx/o3_mass_conc)

# Validation --------------------------------------------------------------

df <- df_salvo %>% 
  filter(!month %in% 6:9) %>% 
  na.omit()

nrow(df) == 13203 # Number of observations
round(mean(df$o3_mass_conc), 1) == 72.2 # Mean ozone concentration


# Saving df_salvo ---------------------------------------------------------

write_rds(
  df_salvo,
  path = "data/artaxo-salvo-geiger/data-asg.rds", 
  compress = "gz" 
)

write_rds(
  df_salvo_nox,
  path = "data/artaxo-salvo-geiger/data-asg-nox.rds", 
  compress = "gz" 
)

# Transfor (df_model) --------------------------------------------------

df_salvo <- read_rds("data/artaxo-salvo-geiger/data-asg.rds")

df_model <- 
  df_salvo %>%
  filter(dv_o3 == 1) %>%
  group_by(stationname) %>%
  mutate(
    trend = date - lubridate::ymd("2008-11-01"),
    trend = as.numeric(trend)/365.25
  ) %>%
  ungroup() %>%
  filter(!month %in% 6:9) %>% 
  mutate(week = as.factor(week)) %>%
  mutate(
    dv_mon_reg = ifelse(dayofweek == 1 & dv_weekday_regular == 1, 1, 0),
    dv_tue_reg = ifelse(dayofweek == 2 & dv_weekday_regular == 1, 1, 0),
    dv_wed_reg = ifelse(dayofweek == 3 & dv_weekday_regular == 1, 1, 0),
    dv_thu_reg = ifelse(dayofweek == 4 & dv_weekday_regular == 1, 1, 0),
    dv_fri_reg = ifelse(dayofweek == 5 & dv_weekday_regular == 1, 1, 0),
    dv_sat_reg = ifelse(
      dayofweek == 6 & 
        dv_publicholiday == 0 &
        dv_yearendvacation == 0, 
      yes = 1, 
      no = 0
    ),
    dv_sun_reg = ifelse(
      dayofweek == 0 & 
        dv_publicholiday == 0 &
        dv_yearendvacation == 0, 
      yes = 1, 
      no = 0
    ),
    dv_sun_vac = ifelse(
      dayofweek == 0 & 
        dv_yearendvacation == 1 &
        dv_publicholiday == 0,
      yes = 1, 
      no = 0
    ),
    dv_sat_vac = ifelse(
      dayofweek == 6 & 
        dv_yearendvacation == 1 &
        dv_publicholiday == 0,
      yes = 1,
      no = 0
    ),
    dv_week_vac = ifelse(
      dayofweek %in% (1:5) & 
        dv_yearendvacation == 1 &
        dv_publicholiday == 0, 
      yes = 1, 
      no = 0
    )
  ) %>%
  mutate(
    dv_kmregion_am_0_4 = ifelse(congestion_region < 4, 1, 0),
    dv_kmregion_am_4_11 = ifelse(congestion_region >= 4 & 
                                   congestion_region < 11, 1, 0),
    dv_kmregion_am_11_18 = ifelse(congestion_region >= 11 &
                                    congestion_region < 18, 1, 0),
    dv_kmregion_am_18_max = ifelse(congestion_region >= 18, 1, 0)
  ) %>% 
  mutate(
    dv_kmcity_am_0_20 = ifelse(congestion_city < 20, 1, 0),
    dv_kmcity_am_20_50 = ifelse(congestion_city >= 20 &
                                  congestion_city < 50, 1, 0),
    dv_kmcity_am_50_80 = ifelse(congestion_city >= 50 &
                                  congestion_city < 80, 1, 0),
    dv_kmcity_am_80_max = ifelse(congestion_city >= 80, 1, 0)
  ) %>%
  mutate(
    dv_pp_0_0 = ifelse(pp == 0, 1, 0),
    dv_pp_0_5 = ifelse(pp > 0 & pp < 0.5, 1, 0),
    dv_pp_5_20 = ifelse(pp >= 0.5 & pp < 2, 1, 0),
    dv_pp_20_150 = ifelse(pp >= 2, 1, 0)
  ) %>% 
  mutate(siteid = as.factor(siteid)) %>% 
  select(-dv_o3)



df_nox <- read_rds("data/artaxo-salvo-geiger/data-nox.rds")

df_model_nox <- df_nox %>%
  filter(hour %in% 5:11, NOx < 300, NOx > 0) %>%
  select(date, stationname, NOx) %>% 
  group_by(date, stationname) %>% 
  summarise_all(.funs = funs(mean), na.rm = TRUE) %>% 
  ungroup()  %>% 
  right_join(df_model, by = c("stationname", "date")) %>%
  filter(!is.na(o3_mass_conc), !is.na(NOx), o3_mass_conc > 0.1) %>% 
  mutate(indice = NOx/o3_mass_conc)
  

# Saving df_model ---------------------------------------------------------

write_rds(
  df_model,
  path = "data/artaxo-salvo-geiger/data-asg-model.rds", 
  compress = "gz" 
)

write_rds(
  df_model_nox,
  path = "data/artaxo-salvo-geiger/data-asg-model-nox.rds", 
  compress = "gz" 
)
