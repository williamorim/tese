library(tidyverse)

df_model <- read_rds("data/datasus-sim/mort_total.rds") %>% 
  right_join(df_ozonio, by = "date")

