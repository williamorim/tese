fit_lasso <- function(df, station, formula) {
  
  df <- filter(df, stationname == station)
  
  split <- createDataPartition(df$o3_mass_conc, p = 0.8, list = FALSE)
  train <- slice(df, split)
  test <- slice(df, -split)
  
  fitControl <- trainControl(method = "cv", number = 10)
  
  model_lasso <- train(
    formula, 
    train, 
    method = "lasso",
    preProc = c("scale", "center"),
    trControl = fitControl)
  
  coef_lasso <- predict.enet(
    model_lasso$finalModel, 
    type = "coefficients", 
    s = model_lasso$bestTune$fraction, 
    mode = "fraction")$coef
  
  pred_lasso <- predict(model_lasso, test)
  rmse <- sqrt(mean((pred_lasso - test$o3_mass_conc)^2))
  
  return(
    list(
      station = station,
      model = model_lasso,
      coef = coef_lasso,
      pred = pred_lasso,
      rmse = rmse
    )
  )
}
