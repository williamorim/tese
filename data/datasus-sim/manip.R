# Manipulando dados SIM - DOSP

library(tidyverse)

juntar_dfs <- function(ano) {
  
  arq <- paste0("data/datasus-sim/DOSP", ano, ".rds")
  
  read_rds(arq) %>%
    select(
      id = NUMERODO,
      tipo_obito = TIPOBITO,
      data_obito = DTOBITO,
      hora_obito = HORAOBITO,
      natural = NATURAL,
      data_nascimento = DTNASC,
      idade = IDADE,
      sexo = SEXO,
      raca = RACACOR,
      municipio = CODMUNRES,
      estado_civil = ESTCIV,
      escolaridade = ESC,
      obito_investigado = TPPOS,
      causa_basica = CAUSABAS,
      causa_basica_original = CAUSABAS_O
    ) %>% 
    mutate_all(.funs = funs(as.character))
}

df <- map_dfr(2007:2016, juntar_dfs) %>% 
  mutate(
    data_obito = lubridate::dmy(data_obito),
    data_nascimento = lubridate::dmy(data_nascimento)
  ) %>% 
  mutate(
    idade = case_when(
      str_sub(idade, 1, 1) == 4 ~ str_sub(idade, 2, 3),
      str_sub(idade, 1, 1) == 4 ~ paste0(1, str_sub(idade, 2, 3)),
      TRUE ~ "0"
    ),
    idade = as.numeric(idade)
  ) 

write_rds(df, "data/datasus-sim/dosp2007-2016.rds", compress = "gz")

# cid <- foreign::read.dbf("data/datasus-sim/dic/CID10.DBF")
# View(cid)
# 
# cidcap <- foreign::read.dbf("data/datasus-sim/dic/CIDCAP10.DBF")
# View(cidcap)

# Manipulações ------------------------------------------------------------

df <- read_rds("data/datasus-sim/dosp2007-2016.rds")

# Mortalidade total diária

df %>% 
  group_by(data_obito) %>% 
  summarise(n_mortes_geral = n()) %>% 
  rename(date = data_obito) %>% 
  write_rds("data/datasus-sim/mort_total_diaria.rds")

# Mortalidade total diária em idosos

df %>% 
  filter(idade >= 60) %>% 
  group_by(data_obito) %>% 
  summarise(n_mortes_idosos = n()) %>% 
  rename(date = data_obito) %>% 
  write_rds("data/datasus-sim/mort_total_diaria_idosos.rds")

# Mortalidade total diária em crianças

df %>% 
  filter(idade <= 5) %>% 
  group_by(data_obito) %>% 
  summarise(n_mortes_criancas = n()) %>% 
  rename(date = data_obito) %>% 
  write_rds("data/datasus-sim/mort_total_diaria_criancas.rds")

# Data.frame para modelagem (Salvo, 2017)

df_salvo <- read_rds("data/artaxo-salvo-geiger/dados_originais.rds")

df_salvo <- df_salvo %>%
  filter(siteid == 1) %>% 
  select(
    date, year, month, week, day, dayofweek,
    dv_publicholiday, dv_weekday_regular, dv_yearendvacation,
    share_gas,
    tp, hm, pp
  ) %>% 
  mutate(
    date = str_c(year, month, day, sep = "-") %>% lubridate::ymd()
  )

mean_vars <- df_salvo %>%
  select(date, tp, hm, pp, share_gas) %>% 
  group_by(date) %>% 
  summarise_all(.funs = funs(mean), na.rm = TRUE) %>% 
  ungroup()

other_vars <- df_salvo %>% 
  select(
    date,
    year:dayofweek, 
    starts_with("dv_")
  ) %>% 
  group_by(date) %>% 
  summarise_all(.funs = funs(first)) %>% 
  ungroup()

df_model <- inner_join(mean_vars, other_vars, by = c("date"))

df_model <- df_model %>%
  mutate(
    trend = date - lubridate::ymd("2008-11-01"),
    trend = as.numeric(trend)/365.25
  ) %>%
  ungroup() %>%
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
    dv_pp_0_0 = ifelse(pp == 0, 1, 0),
    dv_pp_0_5 = ifelse(pp > 0 & pp < 0.5, 1, 0),
    dv_pp_5_20 = ifelse(pp >= 0.5 & pp < 2, 1, 0),
    dv_pp_20_150 = ifelse(pp >= 2, 1, 0)
  )

df_mort <- read_rds("data/datasus-sim/mort_total_diaria.rds")
df_mort_idosos <- read_rds("data/datasus-sim/mort_total_diaria_idosos.rds")
df_mort_criancas <- read_rds("data/datasus-sim/mort_total_diaria_criancas.rds")

inner_join(df_model, df_mort, by = c("date")) %>% 
  inner_join(df_mort_idosos, by = c("date")) %>% 
  inner_join(df_mort_criancas, by = c("date")) %>% 
  write_rds("data/datasus-sim/model_mort_diaria_salvo.rds")


