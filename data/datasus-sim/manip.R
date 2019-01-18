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

RMSP <- c(
  "355030",
  "351880",
  "354780",
  "354880",
  "352940",
  "351380",
  "353440",
  "354870"
)

df <- df %>% 
  filter(municipio %in% RMSP)

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

# Mortalidade (cardiopulmonar + câncer pulmão) em idosos

df %>% 
  filter(idade >= 60) %>%
  filter(str_detect(causa_basica, "I|J") | causa_basica == "C349") %>%
  group_by(data_obito) %>% 
  summarise(n_mortes_idosos2 = n()) %>% 
  rename(date = data_obito) %>% 
  write_rds("data/datasus-sim/mort_doencas_idosos.rds")

# Mortalidade (cardiopulmonar + câncer pulmão) em crianças

df %>% 
  filter(idade <= 5) %>%
  filter(str_detect(causa_basica, "I|J") | causa_basica == "C349") %>% 
  group_by(data_obito) %>% 
  summarise(n_mortes_criancas2 = n()) %>% 
  rename(date = data_obito) %>% 
  write_rds("data/datasus-sim/mort_doencas_criancas.rds")

# Data.frame para modelagem (Salvo, 2017)

df_salvo <- read_rds("data/artaxo-salvo-geiger/dados_originais.rds")

# Ozônio médio na cidade

ozonio <- df_salvo %>% 
  group_by(date, hour) %>% 
  summarise(
    o3_mass_conc = mean(o3_mass_conc, na.rm = TRUE)
  ) %>% 
  .$o3_mass_conc

df_salvo <- df_salvo %>%
  filter(siteid == 1) %>% 
  select(
    date, year, month, week, day, dayofweek, dv_publicholiday,
    share_gas,
    tp, hm,
    o3_mass_conc
  ) %>% 
  mutate(
    o3_mass_conc = ozonio,
    date = str_c(year, month, day, sep = "-") %>% lubridate::ymd(),
    dv_workday = ifelse(
      dayofweek != c(0, 6) & dv_publicholiday == 0, 1, 0
    )
  )

mean_vars <- df_salvo %>%
  select(date, hm, share_gas, o3_mass_conc) %>% 
  group_by(date) %>% 
  summarise_all(.funs = funs(mean), na.rm = TRUE) %>% 
  ungroup()

temperaturas <- df_salvo %>% 
  group_by(date) %>% 
  summarise(
    tp_min = min(tp, na.rm = TRUE),
    tp_max = max(tp, na.rm = TRUE),
    tp_var = tp_max - tp_min,
    tp = mean(tp, na.rm = TRUE)
  ) %>% 
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

df_model <- inner_join(mean_vars, other_vars, by = c("date")) %>% 
  inner_join(temperaturas, by = c("date"))

df_model <- df_model %>%
  mutate(
    trend = date - lubridate::ymd("2008-11-01"),
    trend = as.numeric(trend)/365.25
  ) %>%
  mutate(
    week = as.factor(week),
    dayofweek = as.factor(dayofweek),
    month = as.factor(month),
    season = case_when(
      month %in% 1:3 ~ "verao",
      month %in% 4:6 ~ "outono",
      month %in% 7:9 ~ "inverno",
      month %in% 10:12 ~ "primavera"
    )
  ) 

df_mort <- read_rds("data/datasus-sim/mort_total_diaria.rds")
df_mort_idosos <- read_rds("data/datasus-sim/mort_total_diaria_idosos.rds")
df_mort_criancas <- read_rds("data/datasus-sim/mort_total_diaria_criancas.rds")
df_mort_idosos2 <- read_rds("data/datasus-sim/mort_doencas_idosos.rds")
df_mort_criancas2 <- read_rds("data/datasus-sim/mort_doencas_criancas.rds")

inner_join(df_model, df_mort, by = c("date")) %>% 
  inner_join(df_mort_idosos, by = c("date")) %>% 
  inner_join(df_mort_criancas, by = c("date")) %>% 
  inner_join(df_mort_idosos2, by = c("date")) %>% 
  inner_join(df_mort_criancas2, by = c("date")) %>% 
  write_rds("data/datasus-sim/model_mort_diaria_salvo.rds")


