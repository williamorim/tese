# Consolidando os dados ---------------------------------------------------

files <- list.files(path = "data/cetesb-greve-caminhoneiros/")
paths <- str_c("data/cetesb-greve-caminhoneiros/", files)
paths <- paths[str_detect(paths, ".csv")]

df <- map_dfr(
  paths, 
  read_csv,
  col_types = str_c(rep("c", 17), collapse = "")
)

aux <- df %>%
  select(
    parameter = `Nome Parâmetro`,
    stationname = `Nome Estação`,
    date = Data,
    hour = Hora,
    mass_conc = `Média Horária`
  ) %>%
  mutate(
    mass_conc = str_replace(mass_conc, ",", "."),
    mass_conc = as.numeric(mass_conc),
    date = lubridate::dmy(date),
    hour = str_sub(hour, start = 1, end = 2),
    hour = as.numeric(hour),
    dayofweek = lubridate::wday(date, label = TRUE),
    mass_conc = ifelse(abs(mass_conc) == 9999999, NA, mass_conc),
    parameter = str_replace_all(parameter, " [(].*", "")
  ) %>% 
  spread(parameter, mass_conc)

write_rds(
  aux,
  path = "data/cetesb-greve-caminhoneiros/data-greve-caminhoneiros.rds",
  compress = "gz"
)

# Unidades de medida

# 1 MP10 (Partículas Inaláveis)        µg/m3            
# 2 NO2 (Dióxido de Nitrogênio)        µg/m3            
# 3 CO (Monóxido de Carbono)           ppm              
# 4 NO (Monóxido de Nitrogênio)        µg/m3            
# 5 MP2.5 (Partículas Inaláveis Finas) µg/m3            
# 6 O3 (Ozônio)                        µg/m3  
