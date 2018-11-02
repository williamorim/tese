# Libraries ---------------------------------------------------------------

library(tidyverse)

# Dados -------------------------------------------------------------------

df_model <- read_rds("data/datasus-sim/model_mort_diaria_salvo.rds")

df_model_longo <- df_model %>%
  gather(grupo, n_mortes, starts_with("n_mortes")) %>%
  mutate(
    grupo = case_when(
      grupo == "n_mortes_criancas" ~ "Crianças",
      grupo == "n_mortes_idosos" ~ "Idosos",
      grupo == "n_mortes_geral" ~ "Geral",
    ),
    grupo = forcats::lvls_reorder(grupo, c(1, 3, 2))
  )

# Série -------------------------------------------------------------------

df_model_longo %>% 
  ggplot(aes(x = date, y = n_mortes)) +
  geom_line(aes(color = grupo), show.legend = FALSE) +
  geom_smooth(se = FALSE, method = "lm", show.legend = FALSE, color = "grey") +
  facet_wrap(~grupo, scales = "free") +
  theme_bw()


# Associação --------------------------------------------------------------

# share
df_model_longo %>% 
  ggplot(aes(x = share_gas, y = n_mortes)) +
  geom_point() +
  facet_wrap(~grupo, scales = "free") +
  theme_bw()

# temperatura
df_model_longo %>% 
  ggplot(aes(x = tp, y = n_mortes)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_wrap(~grupo, scales = "free") +
  theme_bw()

# umidade
df_model_longo %>% 
  ggplot(aes(x = hm, y = n_mortes)) +
  geom_point() +
  facet_wrap(~grupo, scales = "free") +
  theme_bw()

# precipitação (num)
df_model_longo %>% 
  ggplot(aes(x = pp, y = n_mortes)) +
  geom_point() +
  facet_wrap(~grupo, scales = "free") +
  theme_bw()

# precipitação (cat)
df_model_longo %>% 
  mutate(pp_cat = ifelse(pp == 0, "Sim", "Não")) %>% 
  ggplot(aes(x = pp_cat, y = n_mortes)) +
  geom_boxplot() +
  facet_wrap(~grupo, scales = "free") +
  theme_bw()

# dia da semana
df_model_longo %>%
  mutate(
    dayofweek = lubridate::wday(date, label = TRUE)
  ) %>% 
  group_by(dayofweek,grupo) %>% 
  summarise(n_mortes = mean(n_mortes, na.rm = TRUE))

df_model_longo %>%
  mutate(
    dayofweek = lubridate::wday(date, label = TRUE)
  ) %>% 
  group_by(dayofweek, grupo) %>% 
  summarise(n_mortes = mean(n_mortes, na.rm = TRUE)) %>% 
  ggplot(aes(x = dayofweek, y = n_mortes)) +
  geom_col(fill = "#ab11e1") +
  facet_wrap(~grupo, scales = "free") +
  theme_bw()

# mês
df_model_longo %>%
  group_by(month, grupo) %>% 
  summarise(n_mortes = mean(n_mortes, na.rm = TRUE))

df_model_longo %>%
  group_by(month, grupo) %>% 
  summarise(n_mortes = mean(n_mortes, na.rm = TRUE)) %>% 
  ggplot(aes(x = month, y = n_mortes)) +
  geom_col(fill = "#ab11e1") +
  scale_x_continuous(breaks = 1:12) +
  facet_wrap(~grupo, scales = "free") +
  theme_bw()

# ano
df_model_longo %>%
  group_by(year, grupo) %>% 
  summarise(n_mortes = mean(n_mortes, na.rm = TRUE))

df_model_longo %>%
  group_by(year, grupo) %>% 
  summarise(n_mortes = mean(n_mortes, na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = n_mortes)) +
  geom_col(fill = "#ab11e1") +
  facet_wrap(~grupo, scales = "free") +
  scale_x_continuous(breaks = 1:12) +
  theme_bw()

# Share -------------------------------------------------------------------

df_model %>% 
  group_by(year) %>% 
  mutate(n = 1:n()) %>% 
  ggplot(aes(x = n, y = share_gas)) +
  geom_line() +
  facet_wrap(~year, nrow = 6)
