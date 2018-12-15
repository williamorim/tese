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
  filter(param_abbrev %in% c("CO", "MP10", "NO", "NO2", "O3")) %>% 
  select(id) %>% 
  flatten_dbl()

safe_scraper_cetesb <- possibly(
  function(station, parameter, start, end) {
    koffing::scraper_cetesb(
      station = station,
      parameter = parameter,
      start = start,
      end = end,
      login = "",
      password = ""
    )
  },
  otherwise = NULL
)

get_cetesb_data <- function(station, parameter) {
  
  start <- "01/09/2018"
  end <- "30/11/2018"
  
  df <- safe_scraper_cetesb(station, parameter, start, end)

  if(!is.null(df)) {
    
    write_csv(
      x = df, 
      path = str_c(
        "data/cetesb-eleicoes/df-", 
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

for(j in 1:length(parameters_id)) {
  walk(
    stations_ids$id, 
    get_cetesb_data,
    parameters_id[j]
  )
}

# Consolidando os dados ---------------------------------------------------

files <- list.files(path = "data/cetesb-eleicoes/")
paths <- str_c("data/cetesb-eleicoes/", files)
paths <- paths[str_detect(paths, ".csv")]

df <- map_dfr(
  paths, 
  read_csv
)

df %>%
  mutate(
    periodo = case_when(
      date == "2018-10-07" | date == "2018-10-28" ~ "eleicao",
      date == "2018-09-30" | date == "2018-09-23" ~ "antes",
      date == "2018-10-14" | date == "2018-10-21" ~ "entre",
      date == "2018-11-04" | date == "2018-11-11" ~ "depois",
      TRUE ~ "outro"
    )
  ) %>% 
  write_rds(
    path = "data/cetesb-eleicoes/data-eleicoes.rds",
    compress = "gz"
  )

# Unidades de medida
  
# 1 MP10 (Partículas Inaláveis)        µg/m3            
# 2 NO2 (Dióxido de Nitrogênio)        µg/m3            
# 3 CO (Monóxido de Carbono)           ppm              
# 4 NO (Monóxido de Nitrogênio)        µg/m3            
# 5 MP2.5 (Partículas Inaláveis Finas) µg/m3            
# 6 O3 (Ozônio)                        µg/m3  
