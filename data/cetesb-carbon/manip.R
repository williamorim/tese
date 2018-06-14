# Pegando os dados de monóxido de carbono do Qualar usando a 
# função koffing::scraper_cetesb()


library(tidyverse)


# Baixando os dados -------------------------------------------------------

stations <- c(
  "Parque D.Pedro II",
  "Mooca",
  "Ibirapuera",
  "São Caetano do Sul",
  "Congonhas",
  "Cerqueira César",
  "Osasco",
  "Taboão da Serra",
  "Pinheiros",
  "Parelheiros",
  "Cid.Universitária-USP-Ipen"
)

stations_ids <- filter(koffing::cetesb_station_ids, stationname %in% stations)

carbon_id <- 
  koffing::cetesb_param_ids %>%
  filter(param_abbrev == "CO") %>% 
  select(id) %>% 
  flatten_dbl()

get_cetesb_data <- function(station, parameter, year) {
  
  start <- str_c("01/01/", year)
  end <- str_c("31/05/", year)
  
  df <- koffing::scraper_cetesb(
    station = station,
    parameter = parameter,
    start = start,
    end = end,
    login = "",
    password = ""
  )
  
  if(!is.null(df)) {
    
    write_csv(
      x = df, 
      path = str_c("data/cetesb-carbon/carbon_", station, "-", year, ".csv")
    )
    
    print(
      str_c("Dados da estação ", station, "de ", year, " baixados com sucesso.")
    )
  }
  
}

years <- 2018

for(i in 1:length(years)) {
  walk(
    stations_ids$id[3], 
    get_cetesb_data, 
    parameter = carbon_id, 
    year = years[i]
  )
}
# Não conseguiu baixar para IPEN em 2016
# Unidade de medida: ppm

# Consolidando os dados ---------------------------------------------------

files <- list.files(path = "data/cetesb-carbon/carbon_cetesb/")
paths <- str_c("data/cetesb-carbon/carbon_cetesb/", files)

df_carbon <- map_dfr(
  paths, 
  read_csv,
  col_types = paste0("????c", paste0(rep("?", 12), collapse = ""))
)

df_carbon %>%
  select(
    stationname = `Nome Estação`,
    date = Data,
    hour = Hora,
    co_mass_conc = `Média Horária`
  ) %>%
  mutate(
    co_mass_conc = str_replace(co_mass_conc, ",", "."),
    co_mass_conc = as.numeric(co_mass_conc),
    date = lubridate::dmy(date),
    hour = str_sub(hour, start = 1, end = 2),
    hour = as.numeric(hour)
  ) %>% 
  write_rds(
    path = "data/cetesb-carbon/data-carbon-2008-2018.rds",
    compress = "gz"
  )

# Outras modificações

read_rds("data/cetesb-carbon/data-carbon-2008-2018.rds") %>%
  mutate(
    # dayofweek = lubridate::wday(date, label = TRUE),
    # stationname = ifelse(
    #   stationname == "Cid.Universitária-USP-Ipen", 
    #   "IPEN-USP",
    #   stationname
    # ),
    co_mass_conc = ifelse(abs(co_mass_conc) == 9999999, NA, co_mass_conc)
  ) %>% 
  write_rds(
    path = "data/cetesb-carbon/data-carbon-2008-2018.rds",
    compress = "gz"
  )

