# Pegando os dados de monóxido de carbono do Qualar usando a 
# função koffing::scraper_cetesb()

library(tidyverse)

# Baixando os dados -------------------------------------------------------

stations <- c(
  "Parque D.Pedro II",
  "Ibirapuera",
  "Osasco",
  "Pinheiros"
)

stations_ids <- filter(koffing::cetesb_station_ids, stationname %in% stations)

parameters_id <- 
  koffing::cetesb_param_ids %>%
  filter(param_abbrev %in% c("CO", "MP10", "MP2.5", "NO", "NO2", "O3")) %>% 
  select(id) %>% 
  flatten_dbl()

safe_scraper_cetesb <- possibly(
  function(station, parameter, start, end) {
    koffing::scraper_cetesb(
      station = station,
      parameter = parameter,
      start = start,
      end = end,
      login = "seu_login",
      password = "sua_senha"
    )
  },
  otherwise = NULL
)

get_cetesb_data <- function(station, parameter, year) {
  
  start <- str_c("01/05/", year)
  end <- str_c("14/06/", year)
  
  df <- safe_scraper_cetesb(station, parameter, start, end)

  if(!is.null(df)) {
    
    write_csv(
      x = df, 
      path = str_c(
        "data/cetesb-greve-caminhoneiros/df-", 
        parameter, "-", station, "-", year, ".csv"
      )
    )
    
    print(
      str_c(
        "Dados de ", parameter, " da estação ", 
        station, " de ", year, " baixados com sucesso."
        )
    )
  } else {
    print(
      str_c(
        "Não foi possível baixar dados de ", parameter, " da estação ", 
        station, " de ", year
      )
    )
  }
  
}

years <- 2016:2018

for(i in 1:length(years)) {
  for(j in 1:length(parameters_id))
  walk(
    stations_ids$id, 
    get_cetesb_data, 
    parameter = parameters_id[j], 
    year = years[i]
  )
}


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
