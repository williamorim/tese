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

df <- map_dfr(2007:2016, juntar_dfs) 

write_rds(df, "data/datasus-sim/dosp2007-2016.rds", compress = "gz")


cid <- foreign::read.dbf("data/datasus-sim/dic/CID10.DBF")
View(cid)

cidcap <- foreign::read.dbf("data/datasus-sim/dic/CIDCAP10.DBF")
View(cidcap)
