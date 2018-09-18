# Consolidando dados de PM baixados do Qualar

library(tidyverse)

files <- list.files(
  path = "data/particulate-matter/", 
  pattern = ".csv",
  full.names = TRUE
)

files <- files[file.info(files)$size > 1700]

df <- purrr::map_dfr(
  files, 
  read_csv
)

df %>%
  mutate(mass_conc = ifelse(mass_conc == 999, NA, mass_conc)) %>% 
  rename(datetime = time) %>% 
  select(-mass_conc_movel) %>% 
  write_rds(
    path = "data/particulate-matter/pm.rds",
    compress = "gz"
  )


# Unidades de medida

# 1 MP10 (Partículas Inaláveis)        µg/m3          
# 2 MP2.5 (Partículas Inaláveis Finas) µg/m3
