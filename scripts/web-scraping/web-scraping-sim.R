# Web scraping do sistema de informações sobre mortalidade (SIM/DATASUS)

library(magrittr)

get_sim_data <- function(base, uf, ano) {
  
  url <- paste0(
    "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DORES/",
    base,
    uf,
    ano, 
    ".dbc"
  )
  
  download.file(url, destfile = "data/datasus-sim/temp.dbc")
  
  read.dbc::read.dbc(file = "data/datasus-sim/temp.dbc") %>% 
    readr::write_rds(
      path = paste0("data/datasus-sim/", base, uf, ano, ".rds"), 
      compress = "gz"
    )
    
}

walk(2007:2016, get_sim_data, base = "DO", uf = "SP")


