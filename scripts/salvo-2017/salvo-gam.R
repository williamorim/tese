# Ozônio - Salvo, 2017
# GAM

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(mgcv)
library(caret)
library(recipes)

source("scripts/salvo-2017/gam-plot.R")

# Dados -------------------------------------------------------------------

df_model <- read_rds("data/artaxo-salvo-geiger/data-asg-model.rds")
df_share_bs <- read_rds("data/artaxo-salvo-geiger/dados_gas_200_bs.rds")

# Formula -----------------------------------------------------------------

df_model <- 
  recipe(df_model) %>% 
  step_dummy(stationname) %>% 
  step_interact(
    terms = ~ matches("^stationname"):trend +
      matches("^stationname"):dv_beltway_open
  ) %>% 
  prep(training = df_model) %>% 
  bake(newdata = df_model)

formula <- df_model %>%
  select(
    -siteid,
    -date, -o3_mass_conc, -dayofweek,
    -starts_with("congestion"),
    -dv_kmregion_am_18_max, -dv_kmcity_am_80_max,
    -pp, -dv_pp_20_150,
    -dv_sun_reg,
    -year, -month, -day, -dv_weekday_regular, -dv_yearendvacation, -dv_o3
  ) %>%
  names() %>%
  str_c(collapse = " + ") %>%
  str_c("o3_mass_conc ~ ", .) %>%
  as.formula()

train_control <- trainControl(method="cv", number = 5)

# Model -------------------------------------------------------------------

# Normal

set.seed(5893524)

model <- train(
  form = formula,
  data = na.omit(df_model),
  method = "gam",
  trControl = train_control
)

model
# RMSE: 19.81
# MAE: 14.91
# % var: 70.50%  

summary(model$finalModel)  
varImp(model)

gam_plot(
  model$finalModel, 
  model$finalModel$smooth[[1]],
  xlab = "Proporção estimada de carros a gasolina"
)
ggsave(filename = "text/figuras/cap-comb-gam-plot.pdf", width = 6, height = 4)

# GAMMA (log)

set.seed(5893524)

model <- train(
  form = formula, 
  data = na.omit(df_model), 
  method = "gam",
  family = Gamma(link = log),
  trControl = train_control
)

model
# RMSE: 20.07
# MAE: 14.95
# % var: 69.50%  

summary(model$finalModel)
varImp(model$finalModel) %>% 
  rownames_to_column() %>% 
  arrange(desc(Overall))


# Normal Inversa (log)

model <- train(
  form = formula, 
  data = na.omit(df_model), 
  method = "gam",
  family = inverse.gaussian(link = "1/mu^2"),
  trControl = train_control
)

model
summary(model$finalModel)   # Precisa rodar
varImp(model)

# Bootstrapping -----------------------------------------------------------

source("scripts/salvo-2017/salvo-bootstrapping.R")

fit_func <- function(data, formula, i) {
  
  model <- train(
    form = formula,
    data = na.omit(data),
    method = "gam",
    tuneGrid = data.frame(select = TRUE, method = "GCV.Cp"),
    trControl = trainControl(method = "none")
  )
  
  write_rds(
    model$finalModel,
    paste0("scripts/salvo-2017/gam-fits/gam_fit_", i, ".rds"),
    compress = "gz"
  )
  
}

walk(
  1:200,
  bootstrapping,
  df = df_model,
  df_share_bs = df_share_bs,
  fit_func = fit_func,
  formula = formula
)

# Plot

df_gam_plot <- map_dfr(
  1:200,
  bs_df_gam_plot
)

ggplot(df_gam_plot, aes(x = x.val, y = value)) +
  geom_line(aes(group = int), color = 'grey', alpha = 0.3) +
  geom_smooth() +
  labs(y = 's(.)', x = "Proporção estimada de carros a gasolina") +
  theme_bw()
ggsave(filename = "text/figuras/cap-comb-gam-plot-bs.pdf", width = 6, height = 4)

