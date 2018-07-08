fit_linear_reg <- function(df, station, formula) {
  
  model <- train(
    form = formula, 
    data = na.omit(df_model), 
    method = "lm", 
    trControl = train_control
  )
  
  coef_lm <- coef(model_lm)
  
  resid_lm <- residuals(model_lm)
  
  pred_lm <- predict(model_lm, test)
  rmse <- sqrt(mean((pred_lm - test$o3_mass_conc)^2, na.rm = TRUE))
  
  return(
    list(
      station = station,
      model = model,
      coef = coef_lm,
      resid = resid_lm,
      pred = pred_lm,
      rmse = rmse
    )
  )
}