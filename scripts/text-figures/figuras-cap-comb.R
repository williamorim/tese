# Tese de Doutorado
# Estratégias para análise de dados de poluição do ar
# Abril de 2017
# William Nilson de Amorim

library(tidyverse)
library(ggridges)
set.seed("5893524")

# Climate graphics ----------------------------------------------

df_O3 <- readr::read_rds("scripts/data/df_O3_clima_2008_2013.rds")

df_tarde_DPII <- df_O3 %>%
  filter(siteid == 1, hour %in% 12:16) %>% 
  group_by(date) %>%
  summarise_at(vars(o3_mass_conc, 
                    tp, rd, hm, ws, pp,
                    dv_ti_0to199m_9am, 
                    dv_ti_200to499m_9am),
               funs(mean),
               na.rm = TRUE) %>% 
  ungroup() %>% 
  mutate(month = lubridate::month(date),
         month = as.factor(month))

medidas_manha <- df_O3 %>%
  filter(siteid == 1, hour %in% 6:10) %>% 
  group_by(date) %>%
  summarise_at(vars(tp, rd, hm, ws, pp),
               funs(mean),
               na.rm = TRUE)  %>%
  rename(morning_tp = tp,
         morning_rd = rd,
         morning_hm = hm,
         morning_ws = ws,
         morning_pp = pp)

df_tarde_DPII <- df_tarde_DPII %>%  
  left_join(medidas_manha, by = "date")

source("scripts/utils/climate-graphs.R")

# temperature-ridge-graph

ridges_plot(df_tarde_DPII, month, vars(tp, morning_tp), 
            xlab = "Celsius", ylab = "Mês",
            facets_names = c(tp = "Temperatura pela tarde",
                             morning_tp = "Temperatura pela manhã"))
ggsave(filename = "figuras/cap-comb-temperature-ridge-graph.pdf", 
       width = 6, height = 4)


# series-temperature-ozone

series_plot(df_tarde_DPII, date, vars(tp, morning_tp, o3_mass_conc),
            xlab = "Ano", ylab = "Valor",
            facets_names = c(tp = "Temperatura pela tarde",
                             morning_tp = "Temperatura pela manhã",
                             o3_mass_conc = "Ozônio"))
ggsave(filename = "figuras/cap-comb-series-temperature-ozone.pdf", 
       width = 6, height = 4)


# scatter-temperature-ozone

scatter_plot(df_tarde_DPII, o3_mass_conc, vars(tp, morning_tp),
             xlab = "Celsius", 
             ylab = expression(paste(O[3], " (", mu, "g/", m^3, ")")),
             facets_names = c(tp = "Temperatura pela tarde",
                              morning_tp = "Temperatura pela manhã"))
ggsave(filename = "figuras/cap-comb-scatter-temperature-ozone.pdf", 
       width = 6, height = 4)


# Traffic graphics ----------------------------------------------

# Data

df_O3 <- readr::read_rds("scripts/data/df_O3_combustiveis_2008_2013.rds")

df_tarde_DPII <- df_O3 %>%
  dplyr::filter(siteid == 1, hour %in% 12:16) %>% 
  dplyr::group_by(date) %>%
  dplyr::summarise_at(.vars = vars(dayofweek, o3_mass_conc, congestion_city,
                                   congestion_region, share_gas),
                      .funs = funs(mean),
                      na.rm = TRUE) %>%  
  dplyr::ungroup()

medidas_manha <- df_O3 %>%
  dplyr::filter(siteid == 1, hour %in% 6:10) %>% 
  dplyr::group_by(date) %>%
  dplyr::summarise_at(.vars = vars(congestion_region, congestion_city),
                      .funs = funs(mean),
                      na.rm = TRUE)  %>%
  rename(morning_congestion_city = congestion_city,
         morning_congestion_region = congestion_region)

df_tarde_DPII <- df_tarde_DPII %>%  
  left_join(medidas_manha, by = "date") %>% 
  rename(z_congestion_city = congestion_city,
         z_congestion_region = congestion_region)

# series-share-ozone

series_plot(df_tarde_DPII, date, vars(share_gas, o3_mass_conc),
            xlab = "Ano", ylab = "Valor",
            facets_names = c(share_gas = "Proporção de carros a gasolina",
                             o3_mass_conc = "Ozônio"))
ggsave(filename = "figuras/cap-comb-series-share-ozone.pdf", 
       width = 6, height = 4)

# scatter-share-ozone

scatter_plot(df_tarde_DPII, o3_mass_conc, 
             vars(share_gas),
             xlab = "%", 
             ylab = expression(paste(O[3], " (", mu, "g/", m^3, ")")),
             facets_names = c(share_gas = "Proporção de carros a gasolina"))
ggsave(filename = "figuras/cap-comb-scatter-share-ozone.pdf", 
       width = 6, height = 4)

# scatter-congestion-city-ozone

scatter_plot(df_tarde_DPII, o3_mass_conc, 
             vars(morning_congestion_city, z_congestion_city),
             xlab = "Quilômetros", 
             ylab = expression(paste(O[3], " (", mu, "g/", m^3, ")")),
             facets_names = c(z_congestion_city = "Trânsito na cidade pela tarde",
                              morning_congestion_city = "Trânsito na cidade pela manhã"))
ggsave(filename = "figuras/cap-comb-scatter-congestion-city-ozone.pdf", 
       width = 6, height = 4)

# scatter-congestion-region-ozone

scatter_plot(df_tarde_DPII, o3_mass_conc, 
             vars(morning_congestion_region, z_congestion_region),
             xlab = "Quilômetros", 
             ylab = expression(paste(O[3], " (", mu, "g/", m^3, ")")),
             facets_names = c(z_congestion_region = "Trânsito pela tarde",
                              morning_congestion_region = "Trânsito pela manhã"))
ggsave(filename = "figuras/cap-comb-scatter-congestion-region-ozone.pdf", 
       width = 6, height = 4)


# ozone-congestion-throughtout-week

dias <- c("Domingo", "Segunda", "Terça", "Quarta", "Quinta", 
          "Sexta", "Sábado")

p1 <- df_tarde_DPII %>% 
  group_by(dayofweek) %>% 
  summarise(o3_mass_conc = mean(o3_mass_conc, na.rm = TRUE)) %>% 
  mutate(dayofweek = as.factor(dayofweek),
         dayofweek = forcats::lvls_revalue(dayofweek, dias),
         dayofweek = forcats::lvls_reorder(dayofweek, c(1, 7, 6:2)),
         o3_mass_conc_lab = round(o3_mass_conc, 1),
         o3_mass_conc_lab = format(o3_mass_conc_lab, nsmall = 1)) %>% 
  ggplot() +
  geom_bar(aes(x = dayofweek, y = o3_mass_conc), 
           stat = "identity", fill = "royalblue") +
  geom_text(aes(x = dayofweek, y = 30, label = dayofweek), color = "white",
            fontface = 2) +
  geom_text(aes(x = dayofweek, y = o3_mass_conc - 5, label = o3_mass_conc_lab),
            color = "white") +
  coord_flip() +
  theme_bw() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        plot.margin = unit(c(0.5, 1, 0.5, 0.5), "cm")) +
  labs(y = expression(paste(O[3], " (", mu, "g/", m^3, ")")), 
       x = "Dia da semana") +
  ggtitle("(a) Concentração de ozônio")
  

p2 <- df_tarde_DPII %>%
  group_by(dayofweek) %>% 
  summarise(congestion_region = mean(z_congestion_region, na.rm = TRUE),
            morning_congestion_region = 
              mean(morning_congestion_region, na.rm = TRUE)) %>% 
  mutate(dayofweek = as.factor(dayofweek),
         dayofweek = forcats::lvls_revalue(dayofweek, dias),
         dayofweek = forcats::lvls_reorder(dayofweek, c(2:7, 1))) %>%
  gather(serie, value, -dayofweek) %>% 
  mutate(serie = forcats::lvls_reorder(serie, c(2, 1))) %>% 
  ggplot() +
  geom_bar(aes(x = dayofweek, y = value, fill = serie), 
           stat = "identity", position = "dodge") +
  theme_bw() +
  theme(legend.position = "top", 
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        plot.margin = unit(c(0.5, 0.5, 0.5, 1), "cm")) +
  scale_fill_discrete(labels = c("Trânsito pela manhã", 
                                "Trânsito pela tarde")) +
  labs(y = "Quilômetros", x = "Dias da semana", fill = "") +
  ggtitle("(b) Trânsito")

p <- gridExtra::marrangeGrob(list(p1, p2), nrow = 1, ncol = 2, top = "")
ggsave(plot = p, 
       filename = "figuras/cap-comb-ozone-congestion-throughtout-week.pdf", 
       width = 8, height = 5)

# ozone-throughtout-week-by-share

dias <- c("Domingo", "Segunda", "Terça", "Quarta", "Quinta", 
          "Sexta", "Sábado")

df_tarde_DPII %>%
  mutate(share_cat = ifelse(share_gas < 0.5, "Álcool", "Gasolina")) %>% 
  group_by(dayofweek, share_cat) %>% 
  summarise(o3_mass_conc = mean(o3_mass_conc, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(dayofweek = as.factor(dayofweek),
         dayofweek = forcats::lvls_revalue(dayofweek, dias),
         dayofweek = forcats::lvls_reorder(dayofweek, c(1, 7, 6:2)),
         o3_mass_conc_lab = round(o3_mass_conc, 1),
         o3_mass_conc_lab = format(o3_mass_conc_lab, nsmall = 1)) %>%
  ggplot() +
  geom_bar(aes(x = dayofweek, y = o3_mass_conc), 
           stat = "identity", fill = "royalblue") +
  geom_text(aes(x = dayofweek, y = 25, label = dayofweek), color = "white",
            fontface = 2) +
  geom_text(aes(x = dayofweek, y = o3_mass_conc - 6, label = o3_mass_conc_lab),
            color = "white") +
  coord_flip() +
  facet_grid(~share_cat) +
  theme_bw() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        plot.margin = unit(c(0.5, 1, 0.5, 0.5), "cm")) +
  labs(y = expression(paste(O[3], " (", mu, "g/", m^3, ")")), 
       x = "Dia da semana")
ggsave(filename = "figuras/cap-comb-ozone-throughtout-week-by-share.pdf", 
       width = 6, height = 4)
