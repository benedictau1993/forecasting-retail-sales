#1 Holt-winters
#2 STL
#3 Prophet
   
#1 building Holt Winters model- weekly data only
Holt_Winters_model <- function(train_series_daily, test_series_daily, train_series_weekly, test_series_weekly, item_names, category_names){
  #packages
  library(forecast)
  library(Metrics)
  #initializing vectors
  product <- item_names
  method <- rep("Holt_Winters", length(train_series_daily))
  seasonality <- c()
  test_mase <- c()
  Ljung_Box_p_value <- c()
  AIC <- c()
  category <- c()
  
  #no daily data
  
  #weekly data 
  for (i in seq_along(train_series_weekly)){
    model_weekly <- suppressWarnings(HoltWinters(ts(train_series_daily[[i]], frequency = 7), seasonal = 'additive'))
    forecast_weekly <- forecast(model_weekly, h = 28)
    mase_weekly <- mase(as.numeric(test_series_weekly[[i]]), as.numeric(forecast_weekly$mean))
    res_weekly <- residuals(forecast_weekly)
    lb_weekly <- Box.test(res_weekly, lag=14, type = "Lj")$p.value
    AIC_weekly <- NA #AIC(model_weekly)
    #storing metrics
    test_mase <- c(test_mase, mase_weekly)
    Ljung_Box_p_value <- c(Ljung_Box_p_value, lb_weekly)
    AIC <- c(AIC, AIC_weekly)
    seasonality <- c(seasonality, "weekly")
    category <- c(category, category_names[i])
  }
  tibble(product = product, category=category, method = method, seasonality = seasonality, test_mase = test_mase,  AIC = AIC, Ljung_Box_p_value = Ljung_Box_p_value)
}


#2. building STL model
stl_model <- function(train_series_daily, test_series_daily, train_series_weekly, test_series_weekly, item_names, category_names){
  #packages
  library(forecast)
  library(Metrics)
  #initializing vectors
  product <- rep(item_names,2)
  method <- rep("STL", 2*length(train_series_daily))
  seasonality <- c()
  test_mase <- c()
  Ljung_Box_p_value <- c()
  AIC <- c()
  category <- c()
  #daily data
  for (i in seq_along(train_series_daily)){
    model_daily <- stl(ts(train_series_daily[[i]],frequency=365), 'periodic')
    forecast_daily <- forecast(model_daily, h = 28)
    mase_daily <- mase(as.numeric(test_series_daily[[i]]), as.numeric(forecast_daily$mean))
    res_daily <- residuals(forecast_daily)
    lb_daily <- Box.test(res_daily, lag=10, type = "Lj")$p.value
    AIC_daily <- NA #AIC(model_daily)########
    #storing metrics
    test_mase <- c(test_mase, mase_daily)
    Ljung_Box_p_value <- c(Ljung_Box_p_value, lb_daily)
    AIC <- c(AIC, AIC_daily)
    seasonality <- c(seasonality, "daily")
  }
  #weekly data 
  for (i in seq_along(train_series_weekly)){
    model_weekly <- stl(train_series_weekly[[i]], 'periodic')
    forecast_weekly <- forecast(model_weekly, h = 28)
    mase_weekly <- mase(as.numeric(test_series_weekly[[i]]), as.numeric(forecast_weekly$mean))
    res_weekly <- residuals(forecast_weekly)
    lb_weekly <- Box.test(res_weekly, lag=14, type = "Lj")$p.value
    AIC_weekly <- NA #AIC(model_weekly)#########
    #storing metrics
    test_mase <- c(test_mase, mase_weekly)
    Ljung_Box_p_value <- c(Ljung_Box_p_value, lb_weekly)
    AIC <- c(AIC, AIC_weekly)
    seasonality <- c(seasonality, "weekly")
    category <- c(category, category_names[i])
  }
  category <- rep(category,2) #repeating because both daily and weekly data
  
  tibble(product = product, category=category, method = method, seasonality = seasonality, test_mase = test_mase,  AIC = AIC, Ljung_Box_p_value = Ljung_Box_p_value)
}



#3. building prophet model
prophet_model <- function(train_series_daily, test_series_daily, train_series_weekly, test_series_weekly, item_names, category_names){
  #packages
  library(forecast)
  library(Metrics)
  library(prophet)
  #initializing vectors
  product <- item_names
  method <- rep("Prophet", length(train_series_weekly))
  seasonality <- c()
  test_mase <- c()
  Ljung_Box_p_value <- c()
  AIC <- c()
  category <- c()
  #weekly data 
  for (i in seq_along(train_series_weekly)){
    model_weekly_df<- as.data.frame(cbind(
      "ds" = as.vector(seq(as.Date("2011-01-29"), as.Date("2016-03-27"), by="days")),
      "y" = as.vector(train_series_weekly[[i]])))
    model_weekly_df[['ds']] <- as.Date(model_weekly_df[['ds']], format='%Y-%m-%d')
    #fitting prophet onto data
    prophet_mod <- suppressWarnings(prophet(model_weekly_df))
    #making future dataframe
    future_df <- suppressWarnings(make_future_dataframe(prophet_mod, periods = 28))
    #creating forecast
    forecast_weekly <- suppressWarnings(predict(prophet_mod, future_df))
    #forecast_weekly <- forecast(model_weekly, h = 28)
    mase_weekly <- mase(as.numeric(test_series_weekly[[i]]), tail(forecast_weekly$yhat[], n=28))
    res_weekly <- head(forecast_weekly$yhat[],1885) - train_series_weekly[[i]]
    lb_weekly <- Box.test(res_weekly, lag=14, type = "Lj")$p.value
    AIC_weekly <- NA #AIC(model_weekly)
    #storing metrics
    test_mase <- c(test_mase, mase_weekly)
    Ljung_Box_p_value <- c(Ljung_Box_p_value, lb_weekly)
    AIC <- c(AIC, AIC_weekly)
    seasonality <- c(seasonality, "weekly")
    category <- c(category, category_names[i])
  }
  tibble(product = product, category=category, method = method, seasonality = seasonality, test_mase = test_mase, AIC = AIC, Ljung_Box_p_value = Ljung_Box_p_value)
}

#4 TBATS model
TBATS_model <- function(train_series_daily, test_series_daily, train_series_weekly, test_series_weekly, item_names, category_names){
  #packages
  library(forecast)
  library(Metrics)
  #initializing vectors
  product <- item_names
  method <- rep("TBATS", length(train_series_daily))
  seasonality <- c()
  test_mase <- c()
  Ljung_Box_p_value <- c()
  AIC <- c()
  category <- c()
  
  #weekly data 
  for (i in seq_along(train_series_weekly)){
    model_weekly <- suppressWarnings(tbats(ts(train_series_daily[[i]], frequency = 7)))
    forecast_weekly <- forecast(model_weekly, h = 28)
    mase_weekly <- mase(as.numeric(test_series_weekly[[i]]), as.numeric(forecast_weekly$mean))
    res_weekly <- residuals(forecast_weekly)
    lb_weekly <- Box.test(res_weekly, lag=14, type = "Lj")$p.value
    AIC_weekly <- NA #AIC(model_weekly)
    #storing metrics
    test_mase <- c(test_mase, mase_weekly)
    Ljung_Box_p_value <- c(Ljung_Box_p_value, lb_weekly)
    AIC <- c(AIC, AIC_weekly)
    seasonality <- c(seasonality, "weekly")
    category <- c(category, category_names[i])
  }
  tibble(product = product, category=category, method = method, seasonality = seasonality, test_mase = test_mase,  AIC = AIC, Ljung_Box_p_value = Ljung_Box_p_value)
}





