# Base original. Acrescentando nome das estações.

site_names <- c("Dom Pedro II", "Santana", "Mooca", "Ibirapuera", 
                "Nossa Senhora do O", "Sao Caetano", "Congonhas", 
                "Cerqueira Cesar", "Diadema", "Santo Andre", 
                "Maua", "Pinheiros", "Parelheiros", "IPEN")

o3_sites <- c(1, 2, 3, 5, 6, 7, 15, 18, 22, 27, 29, 31)

data_path <- "data/artaxo-salvo-geiger/dados_originais.csv"

df_o3 <- readr::read_csv(data_path) %>%
  mutate(date = stringr::str_c(year, month, day, sep = "-"),
         date = lubridate::ymd(date),
         stationname = as.factor(siteid),
         stationname = forcats::lvls_revalue(stationname, site_names),
         stationname = as.character(stationname)) %>% 
  filter(siteid %in% o3_sites) %>% 
  select(stationname,
         date, hour, dayofweek, week, month,
         dv_beltway_open,
         o3_mass_conc, share_gas,
         rd, tp, hm, ws, pp, starts_with("dv_ti"),
         starts_with("congestion"))

# Filtrando os dados segundo a estratégia adotada por Salvo et al (2017).

df_o3_afternoon <- df_o3 %>%
  filter(!month %in% c(6, 7, 8, 9), hour %in% 12:16) %>%
  select(-hour, -starts_with("dv_ti"), -starts_with("congestion")) %>%
  group_by(date, stationname) %>%
  summarise_if(is.numeric, funs(mean), na.rm = TRUE) %>% 
  ungroup()

df_o3_morning <- df_o3 %>%
  filter(!month %in% c(6, 7, 8, 9), hour %in% 7:11) %>%
  select(date, stationname, pp,
         starts_with("dv_ti"), starts_with("congestion")) %>% 
  group_by(date, stationname) %>%
  summarise_if(is.numeric, funs(mean), na.rm = TRUE) %>%
  rename(pp_m = pp) %>% 
  ungroup()

df_salvo <- inner_join(df_o3_afternoon, df_o3_morning) %>% na.omit()

df_salvo <- df_salvo %>%
  group_by(stationname) %>% 
  mutate(trend = 1:n()) %>% 
  ungroup()

write_rds(df_salvo, "analises/data/df_salvo_2017.rds", compress = "gz")
