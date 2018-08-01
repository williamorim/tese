# Pegando os dados de monóxido de carbono do Qualar usando a 
# função koffing::scraper_cetesb()

library(tidyverse)

# Baixando os dados -------------------------------------------------------

stations <- c(
  "Congonhas",
  "Cerqueira César",
  "N.Senhora do Ó",
  "Mooca",
  "Parque D.Pedro II",
  "Osasco",
  "Pinheiros",
  "Parelheiros",
  "Santo Amaro",
  "Interlagos",
  "Cid.Universitária-USP-Ipen",
  "Santana",
  "Ibirapuera"
)

stations_ids <- filter(koffing::cetesb_station_ids, stationname %in% stations)

parameters_id <- 
  koffing::cetesb_param_ids %>%
  filter(param_abbrev %in% c("MP10", "MP2.5")) %>% 
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

get_cetesb_data <- function(station, parameter, year) {
  
  start <- str_c("01/01/", year)
  end <- str_c("31/12/", year)
  
  df <- safe_scraper_cetesb(station, parameter, start, end)
  
  if(!is.null(df)) {
    
    write_csv(
      x = df, 
      path = str_c(
        "data/particulate-matter/df-", 
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

years <- 2008:2017

for(i in 1:length(years)) {
  for(j in 1:length(parameters_id))
    walk(
      stations_ids$id, 
      get_cetesb_data, 
      parameter = parameters_id[j], 
      year = years[i]
    )
}
