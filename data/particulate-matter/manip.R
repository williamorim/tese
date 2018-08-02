# Consolidando dados de PM baixados do Qualar

library(tidyverse)

files <- list.files(path = "data/particulate-matter/")
paths <- str_c("data/particulate-matter/", files)
paths <- paths[str_detect(paths, ".csv")]
paths <- paths[file.info(paths)$size > 1700]

df <- map_dfr(
  paths, 
  read_csv
)

df %>% 
  select(-time, -mass_conc_movel) %>% 
  write_rds(
    path = "data/particulate-matter/pm.rds",
    compress = "gz"
  )

# Unidades de medida

# 1 MP10 (Partículas Inaláveis)        µg/m3            
# 2 NO2 (Dióxido de Nitrogênio)        µg/m3            
# 3 CO (Monóxido de Carbono)           ppm              
# 4 NO (Monóxido de Nitrogênio)        µg/m3            
# 5 MP2.5 (Partículas Inaláveis Finas) µg/m3            
# 6 O3 (Ozônio)                        µg/m3  
