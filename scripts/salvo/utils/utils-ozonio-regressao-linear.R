fit_linear_reg <- function(df, station, formula) {
  
  df <- filter(df, stationname == station)
  
  split <- sample(1:nrow(df), 0.7*nrow(df))
  train <- slice(df, split)
  test <- slice(df, -split)
  
  model_lm <- lm(formula, data = train)
  
  coef_lm <- coef(model_lm)
  
  resid_lm <- residuals(model_lm)
  
  pred_lm <- predict(model_lm, test)
  rmse <- sqrt(mean((pred_lm - test$o3_mass_conc)^2, na.rm = TRUE))
  
  return(
    list(
      station = station,
      model = model_lm,
      coef = coef_lm,
      resid = resid_lm,
      pred = pred_lm,
      rmse = rmse
    )
  )
}