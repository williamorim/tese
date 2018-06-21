# Reproduzindo análise do artigo do Salvo de 2017


# Libraries ---------------------------------------------------------------

library(tidyverse)

# Data --------------------------------------------------------------------

df_salvo <- read_rds("data/artaxo-salvo-geiger/data-asg.rds")

# Transform ---------------------------------------------------------------

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
  mutate(
    stationname = ifelse(stationname == "Dom Pedro II", "a", stationname)
  )




# Formula -----------------------------------------------------------------

formula <- df_model %>%
  select(
    -date, -o3_mass_conc, -dayofweek,
    -starts_with("congestion"),
    -dv_kmregion_am_18_max, -dv_kmcity_am_80_max,
    -pp, -dv_pp_20_150,
    -dv_sun_reg,
    -year, -month, -day, -dv_weekday_regular, -dv_yearendvacation, -dv_o3
  ) %>%
  names() %>%
  stringr::str_c(collapse = " + ") %>%
  stringr::str_c("o3_mass_conc ~ ", .) %>%
  stringr::str_c("+ trend*stationname") %>%
  stringr::str_c("+ dv_beltway_open*stationname") %>%
  as.formula()

# Validation
ncol(model.matrix(formula, df_model)) == 96 # Number of parameters
nrow(na.omit(df_model)) == 13203 # Number of observations
df_model %>% 
  na.omit %>% 
  .$o3_mass_conc %>% 
  mean %>% 
  round(1) %>%
  magrittr::equals(72.2) # Mean ozone concentration


# Model -------------------------------------------------------------------

# Usual lm

fit <- lm(formula, data = df_model)
summary(fit)

# Cluster lm

fit <- miceadds::lm.cluster(formula, data = df_model, cluster = "date")
summary(fit)
