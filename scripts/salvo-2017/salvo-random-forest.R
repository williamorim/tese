# Ozônio - Salvo, 2017
# GAM

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(caret)
library(recipes)
library(lime)
library(patchwork)

source("scripts/salvo-2017/salvo-utils.R")

# Dados -------------------------------------------------------------------

df_model <- read_rds("data/artaxo-salvo-geiger/data-asg-model.rds")

# Formula -----------------------------------------------------------------

formula <- df_model %>%
  select(
    -siteid,
    -date, -o3_mass_conc, -dayofweek,
    -starts_with("congestion"),
    -dv_kmregion_am_18_max, -dv_kmcity_am_80_max,
    -pp, -dv_pp_20_150,
    -dv_sun_reg,
    -year, -month, -day, -dv_weekday_regular, -dv_yearendvacation
  ) %>%
  names() %>%
  str_c(collapse = " + ") %>%
  str_c("o3_mass_conc ~ ", .) %>%
  as.formula()

rec <- 
  df_model %>%
  na.omit() %>% 
  recipe(formula = formula) %>%
  step_naomit(all_predictors(), all_outcomes()) %>% 
  step_dummy(stationname, week, one_hot = TRUE) %>% 
  step_interact(
    terms = ~ matches("^stationname"):trend +
      matches("^stationname"):dv_beltway_open
  )

# Model -------------------------------------------------------------------

train_control <- trainControl(method = "cv", number = 5)

tuning_grid <- expand.grid(
  splitrule = "variance",
  mtry = 48,
  min.node.size = 1
)

# Caret

set.seed(5893524)

model <- train(
  x = rec,
  data = na.omit(df_model),
  method = "ranger",
  trControl = train_control,
  tuneGrid = tuning_grid,
  importance = 'impurity'
)

model
# RMSE: 14.11
# MAE: 10.20
# % var: 85.72%  
# share_gas imp: 6ª

model$finalModel
varImp(model)

pred_obs_plot(
  obs = na.omit(df_model)$o3_mass_conc,
  pred = predict(model, newdata = na.omit(df_model))
)
ggsave(
  filename = "text/figuras/cap-comb-forest-pred-obs-plot.pdf", 
  width = 6, 
  height = 4
)


# Lime --------------------------------------------------------------------

explainer <- lime(
  na.omit(df_model), 
  model
)


# 10% mais poluídos

test_days <- 
  df_model %>% 
  na.omit %>%
  filter(stationname == "Pinheiros") %>%
  top_n(n = 100, o3_mass_conc)

explanation <- explain(
  test_days, 
  explainer,
  n_features = 10
)

p1 <- explanation %>% 
  filter(feature == "share_gas") %>%
  mutate(feature_value = as.numeric(feature_value)) %>% 
  ggplot(aes(x = feature_value, y = feature_weight)) +
  geom_point() +
  labs(
    x = "Proporção de carros a gasolina",
    y = "Coeficiente no modelo simple"
  ) +
  ggtitle("Maiores médias de ozônio")
  theme_bw()

# explanation %>% 
#   filter(feature == "share_gas") %>% 
#   mutate(color = ifelse(feature_weight < 0, "0", "1")) %>% 
#   ggplot(aes(y = feature_weight, x = case, fill = color)) +
#   geom_bar(stat = "identity", show.legend = FALSE, position = "dodge") +
#   coord_flip() +
#   facet_wrap(~feature_desc, scales = "free") +
#   theme_bw()

# 10% menos poluídos

test_days <- 
  df_model %>% 
  na.omit %>%
  filter(stationname == "Pinheiros") %>%
  top_n(n = -100, o3_mass_conc)

explanation <- explain(
  test_days, 
  explainer, 
  n_features = 10
)

p2 <- explanation %>% 
  filter(feature == "share_gas") %>%
  mutate(feature_value = as.numeric(feature_value)) %>% 
  ggplot(aes(x = feature_value, y = feature_weight)) +
  geom_point() +
  labs(
    x = "Proporção de carros a gasolina",
    y = "Coeficiente no modelo simple"
  ) +
  ggtitle("Menores médias de ozônio") +
  theme_bw()

# explanation %>% 
#   ggplot(aes(x = feature)) +
#   geom_bar() +
#   theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
# 
# explanation %>% 
#   filter(feature == "share_gas") %>% 
#   mutate(color = ifelse(feature_weight < 0, "0", "1")) %>% 
#   ggplot(aes(y = feature_weight, x = case, fill = color)) +
#   geom_bar(stat = "identity", show.legend = FALSE, position = "dodge") +
#   coord_flip() +
#   facet_wrap(~feature_desc, scales = "free") 

p2 + p1

ggsave(
  filename = "text/figuras/cap-comb-lime-pinheiros.pdf", 
  width = 6, 
  height = 4
)
