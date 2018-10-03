library(tidyverse)

df_morte <- read_rds("data/datasus-sim/dosp2007-2016.rds")
df_morte %>% View

df_morte$data_obito - df_morte$data_nascimento

df_morte <- df_morte %>% 
  mutate(
    idade = case_when(
      str_sub(idade, 1, 1) == 4 ~ str_sub(idade, 2, 3),
      str_sub(idade, 1, 1) == 4 ~ paste0(1, str_sub(idade, 2, 3)),
      TRUE ~ "0"
    ),
    idade = as.numeric(idade)
  )
