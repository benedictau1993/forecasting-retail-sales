# runs all code from data_cleaning_functions file, creating the functions we need
source("data_cleaning_functions.R")

# to play nice with tidyverse, read in data as tibbles
calendar <- read_csv("m5_data/calendar.csv")
sales_train_validation <- read_csv("m5_data/sales_train_validation.csv")
sell_prices <- read_csv("m5_data/sell_prices.csv")

# you can specify item_ids and states you want here as a character vector
items <- c("HOBBIES_1_209", "HOBBIES_1_330", "HOBBIES_1_043", "HOBBIES_1_130", "HOBBIES_1_150",
           "FOODS_3_586", "FOODS_3_252", "FOODS_3_555", "FOODS_3_101", "FOODS_3_007",
           "HOUSEHOLD_1_027", "HOUSEHOLD_1_474", "HOUSEHOLD_1_099", "HOUSEHOLD_1_125", "HOUSEHOLD_1_129",
           "FOODS_2_197", "FOODS_2_276", "FOODS_2_019", "FOODS_2_181", "FOODS_2_347")




states <- c("CA")

# how each function works is described in the data_cleaning_functions.R file. So go there if you want to see what
# they do and perhaps change how they work.
# otherwise, just run the code below and you should be good to go
item_names <- items
category_names <- generate_dept_ids(items)
tidy_dat <- tidy_up(items, states, sales_train_validation, calendar, sell_prices)
train_series_weekly <- extract_train_weekly(items, tidy_dat)
train_series_daily <- extract_train_daily(items, tidy_dat)
test_series_weekly <- extract_test_weekly(items, tidy_dat)
test_series_daily <- extract_test_daily(items, tidy_dat)
train_xreg <- extract_train_xreg(items, states, tidy_dat)
test_xreg <- extract_test_xreg(items, states, tidy_dat)


# some things to note:
# 1) tidy_up() returns sales data in a wide format (each series in its own column) for the states specified, 
#    aggregated/grouped by item_id
# 1) naming convention for ts objects in train_series_weekly, test_series_weekly etc, is <set_seasonality_item_id>
#    e.g. "train_weekly_FOODS_2_197"
# 2) the series variables are lists containing ts objects. When indexing into these lists
#    (while in a loop, for example), I recommend double square brackets [[]].
#    Because list[[i]] returns the element at index i in the list, while list[i] returns a list with only the element
#    at index i in the list in it. And your models will want the ts object, not a list containing a ts object.
# 3) xreg variables are matrices
# 4) train obs are 1:1885, test obs are 1886:1913
# 5) weekly frequency = 7, daily frequency = 1

-------------

library(forecast)


############## AVERAGE ##############

#building average
avg_model <- function(train_series_daily, test_series_daily, item_names, category_names, train_xreg, test_xreg){
#Note: this model will return the same forecast for both daily and weekly time series
#Note: this model returns NULL for AIC values because the model does not have an AIC value
#packages
  library(forecast)
  library(Metrics)
#initializing vectors
  category <- c(category_names)
  product <- rep(item_names)
  method <- rep("average", length(train_series_daily))
  seasonality <- c()
  test_mase <- c()
  Ljung_Box_p_value <- c()
  Akaike_info_crit <- c()
#daily data
  for (i in seq_along(train_series_daily)){
      forecast_avg_daily <- meanf(train_series_daily[[i]], h=28)
      mase_avg_daily <- mase(as.numeric(test_series_daily[[i]]), as.numeric(forecast_avg_daily$mean))
      res_avg_daily <- residuals(forecast_avg_daily)
      lb_avg_daily <- Box.test(res_avg_daily, lag=10, type = "Lj")$p.value
      AIC_daily <- NA
      #storing metrics
      test_mase <- c(test_mase, mase_avg_daily)
      Ljung_Box_p_value <- c(Ljung_Box_p_value, lb_avg_daily)
      Akaike_info_crit <- c(Akaike_info_crit, AIC_daily)
      seasonality <- c(seasonality, "NA")
  }
output <- tibble(product = product, category = category, method = method, seasonality = seasonality, test_mase = test_mase, Ljung_Box_p_value = Ljung_Box_p_value, Akaike_info_crit = Akaike_info_crit)
#return output
output
}
  
#using avg_model
avg_output <- avg_model(train_series_daily, test_series_daily, item_names, category_names, train_xreg, test_xreg)

############## NAIVE ##############

#building naive
naive_model <- function(train_series_daily, test_series_daily, item_names, category_names, train_xreg, test_xreg){
  #Note: this model will return the same forecast for both daily and weekly time series
  #Note: this model returns NULL for AIC values because the model does not have an AIC value
  #packages
  library(forecast)
  library(Metrics)
  #initializing vectors
  product <- rep(item_names)
  category <- c(category_names)
  method <- rep("naive", length(train_series_daily))
  seasonality <- c()
  test_mase <- c()
  Ljung_Box_p_value <- c()
  Akaike_info_crit <- c()
  #daily data
  for (i in seq_along(train_series_daily)){
    forecast_daily <- naive(train_series_daily[[i]], h=28)
    mase_daily <- mase(as.numeric(test_series_daily[[i]]), as.numeric(forecast_daily$mean))
    res_daily <- residuals(forecast_daily)
    lb_daily <- Box.test(res_daily, lag=10, type = "Lj")$p.value
    AIC_daily <- NA
    #storing metrics
    test_mase <- c(test_mase, mase_daily)
    Ljung_Box_p_value <- c(Ljung_Box_p_value, lb_daily)
    Akaike_info_crit <- c(Akaike_info_crit, AIC_daily)
    seasonality <- c(seasonality, "NA")
  }
  output <- tibble(product = product, category = category, method = method, seasonality = seasonality, test_mase = test_mase, Ljung_Box_p_value = Ljung_Box_p_value, Akaike_info_crit = Akaike_info_crit)
  #return output
  output
}

#using naive_model
naive_output <- naive_model(train_series_daily, test_series_daily, item_names, category_names, train_xreg, test_xreg)

############## SEASONAL NAIVE ##############

#building seasonal naive model
snaive_model <- function(train_series_daily, test_series_daily, item_names, category_names, train_xreg, test_xreg){
  #Note: this model returns NULL for AIC values because the model does not have an AIC value
  #packages
  library(forecast)
  library(Metrics)
  #initializing vectors
  product <- rep(item_names,2)
  category <- rep(category_names,2)
  method <- rep("seasonal_naive", 2*length(train_series_daily))
  seasonality <- c()
  test_mase <- c()
  Ljung_Box_p_value <- c()
  Akaike_info_crit <- c()
  #daily data
  for (i in seq_along(train_series_daily)){
    forecast_daily <- snaive(train_series_daily[[i]], h=28)
    mase_daily <- mase(as.numeric(test_series_daily[[i]]), as.numeric(forecast_daily$mean))
    res_daily <- residuals(forecast_daily)
    lb_daily <- Box.test(res_daily, lag=10, type = "Lj")$p.value
    AIC_daily <- NA
    #storing metrics
    test_mase <- c(test_mase, mase_daily)
    Ljung_Box_p_value <- c(Ljung_Box_p_value, lb_daily)
    Akaike_info_crit <- c(Akaike_info_crit, AIC_daily)
    seasonality <- c(seasonality, "daily")
  }
  #weekly data 
  for (i in seq_along(train_series_weekly)){
    forecast_weekly <- snaive(train_series_weekly[[i]], h=28)
    mase_weekly <- mase(as.numeric(test_series_weekly[[i]]), as.numeric(forecast_weekly$mean))
    res_weekly <- residuals(forecast_weekly)
    lb_weekly <- Box.test(res_weekly, lag=14, type = "Lj")$p.value
    AIC_weekly <- NA
    #storing metrics
    test_mase <- c(test_mase, mase_weekly)
    Ljung_Box_p_value <- c(Ljung_Box_p_value, lb_weekly)
    Akaike_info_crit <- c(Akaike_info_crit, AIC_weekly)
    seasonality <- c(seasonality, "weekly")
  }
  output <- tibble(product = product, category = category,method = method, seasonality = seasonality, test_mase = test_mase, Ljung_Box_p_value = Ljung_Box_p_value, Akaike_info_crit = Akaike_info_crit)
  #return output
  output
}

#using snaive_model
snaive_output <- snaive_model(train_series_daily, test_series_daily, item_names, category_names, train_xreg, test_xreg)
snaive_output
############## ARIMA ##############

#building ARIMA model
ARIMA_model <- function(train_series_daily, test_series_daily,item_names, category_names, train_xreg, test_xreg){
  #packages
  library(forecast)
  library(Metrics)
  #initializing vectors
  product <- rep(item_names)
  category <- c(category_names)
  method <- rep("ARIMA",length(train_series_daily))
  seasonality <- c()
  test_mase <- c()
  Ljung_Box_p_value <- c()
  Akaike_info_crit <- c()
  #daily data
  for (i in seq_along(train_series_daily)){
    model_daily <- auto.arima(train_series_daily[[i]], seasonal = FALSE, method="ML")
    forecast_daily <- forecast(model_daily, h = 28)
    mase_daily <- mase(as.numeric(test_series_daily[[i]]), as.numeric(forecast_daily$mean))
    res_daily <- residuals(forecast_daily)
    lb_daily <- Box.test(res_daily, lag=10, type = "Lj")$p.value
    AIC_daily <- AIC(model_daily)
    #storing metrics
    test_mase <- c(test_mase, mase_daily)
    Ljung_Box_p_value <- c(Ljung_Box_p_value, lb_daily)
    Akaike_info_crit <- c(Akaike_info_crit, AIC_daily)
    seasonality <- c(seasonality, "NA")
  }
  output <- tibble(product = product, category = category,method = method, seasonality = seasonality, test_mase = test_mase, Ljung_Box_p_value = Ljung_Box_p_value, Akaike_info_crit = Akaike_info_crit)
  #return output
  output
}

#using ARIMA_model
ARIMA_output <- ARIMA_model(train_series_daily, test_series_daily, item_names, category_names, train_xreg, test_xreg)
ARIMA_output

############## SARIMA ##############

#building SARIMA model
SARIMA_model <- function(train_series_daily, test_series_daily, train_series_weekly, test_series_weekly, item_names, category_names, train_xreg, test_xreg){
  #packages
  library(forecast)
  library(Metrics)
  #initializing vectors
  product <- rep(item_names,2)
  category <- rep(category_names,2)
  method <- rep("SARIMA", 2*length(train_series_daily))
  seasonality <- c()
  test_mase <- c()
  Ljung_Box_p_value <- c()
  Akaike_info_crit <- c()
  #daily data
  for (i in seq_along(train_series_daily)){
    model_daily <- auto.arima(train_series_daily[[i]], seasonal = TRUE)
    forecast_daily <- forecast(model_daily, h = 28)
    mase_daily <- mase(as.numeric(test_series_daily[[i]]), as.numeric(forecast_daily$mean))
    res_daily <- residuals(forecast_daily)
    lb_daily <- Box.test(res_daily, lag=10, type = "Lj")$p.value
    AIC_daily <- AIC(model_daily)
    #storing metrics
    test_mase <- c(test_mase, mase_daily)
    Ljung_Box_p_value <- c(Ljung_Box_p_value, lb_daily)
    Akaike_info_crit <- c(Akaike_info_crit, AIC_daily)
    seasonality <- c(seasonality, "daily")
  }
  #weekly data 
  for (i in seq_along(train_series_weekly)){
    model_weekly <- auto.arima(train_series_weekly[[i]], seasonal = TRUE)
    forecast_weekly <- forecast(model_weekly, h = 28)
    mase_weekly <- mase(as.numeric(test_series_weekly[[i]]), as.numeric(forecast_weekly$mean))
    res_weekly <- residuals(forecast_weekly)
    lb_weekly <- Box.test(res_weekly, lag=14, type = "Lj")$p.value
    AIC_weekly <- AIC(model_weekly)
    #storing metrics
    test_mase <- c(test_mase, mase_weekly)
    Ljung_Box_p_value <- c(Ljung_Box_p_value, lb_weekly)
    Akaike_info_crit <- c(Akaike_info_crit, AIC_weekly)
    seasonality <- c(seasonality, "weekly")
  }
  output <- tibble(product = product, category = category, method = method, seasonality = seasonality, test_mase = test_mase, Ljung_Box_p_value = Ljung_Box_p_value, Akaike_info_crit = Akaike_info_crit)
  #return output
  output
}

#using SARIMA model
SARIMA_output <- SARIMA_model(train_series_daily, test_series_daily, train_series_weekly, test_series_weekly, item_names, category_names, train_xreg, test_xreg)

############## ARFIMA ##############
#dropping columns to avoid being rank deficient - this can be ignored with Yannik's final code
train_xreg <- train_xreg[, -c(9, 10, 19, 22, 23, 24, 25)]
test_xreg <- test_xreg[, -c(9, 10, 19, 22, 23, 24, 25)]
####################################3

#building ARFIMA
ARFIMA_model <- function(train_series_daily, test_series_daily, train_series_weekly, test_series_weekly, item_names, category_names, train_xreg, test_xreg){
  #packages
  library(forecast)
  library(Metrics)
  #initializing vectors
  product <- rep(item_names,2)
  category <- rep(category_names,2)
  method <- rep("ARFIMA", 2*length(train_series_daily))
  seasonality <- c()
  test_mase <- c()
  Ljung_Box_p_value <- c()
  Akaike_info_crit <- c()
  #daily data
  for (i in seq_along(train_series_daily)){
    model_daily <- arfima(as.numeric(train_series_daily[[i]]), seasonal = TRUE, method = "ML")
    forecast_daily <- forecast(model_daily, h = 28)
    mase_daily <- mase(as.numeric(test_series_daily[[i]]), as.numeric(forecast_daily$mean))
    res_daily <- residuals(forecast_daily)
    lb_daily <- Box.test(res_daily, lag=10, type = "Lj")$p.value
    AIC_daily <- AIC(model_daily)
    #storing metrics
    test_mase <- c(test_mase, mase_daily)
    Ljung_Box_p_value <- c(Ljung_Box_p_value, lb_daily)
    Akaike_info_crit <- c(Akaike_info_crit, AIC_daily)
    seasonality <- c(seasonality, "daily")
  }
  #weekly data 
  for (i in seq_along(train_series_weekly)){
    model_weekly <- arfima(train_series_weekly[[i]], seasonal = TRUE, method='ML')
    forecast_weekly <- forecast(model_weekly, h = 28)
    mase_weekly <- mase(as.numeric(test_series_weekly[[i]]), as.numeric(forecast_weekly$mean))
    res_weekly <- residuals(forecast_weekly)
    lb_weekly <- Box.test(res_weekly, lag=14, type = "Lj")$p.value
    AIC_weekly <- AIC(model_weekly)
    #storing metrics
    test_mase <- c(test_mase, mase_weekly)
    Ljung_Box_p_value <- c(Ljung_Box_p_value, lb_weekly)
    Akaike_info_crit <- c(Akaike_info_crit, AIC_weekly)
    seasonality <- c(seasonality, "weekly")
  }
  output <- tibble(product = product, category = category,method = method, seasonality = seasonality, test_mase = test_mase, Ljung_Box_p_value = Ljung_Box_p_value, Akaike_info_crit = Akaike_info_crit)
  #return output
  output
}  
  
#using ARFIMA 
ARFIMA_output <- ARFIMA_model(train_series_daily, test_series_daily, train_series_weekly, test_series_weekly, item_names, category_names, train_xreg, test_xreg)


############## REGRESSION ##############
tidy_dat_train <- tidy_dat[1:1885,]
tidy_dat_test <- tidy_dat[1886:1913,]


#building regression
reg_model <- function(tidy_dat_train, tidy_dat_test, item_names, category_names, train_xreg, test_xreg){
  #packages
  library(forecast)
  library(Metrics)
  #initializing vectors
  product <- rep(item_names)
  category <- c(category_names)
  method <- rep("regression", length(item_names))
  seasonality <- c()
  test_mase <- c()
  Ljung_Box_p_value <- c()
  Akaike_info_crit <- c()
  form <- paste0(item_names, "~ .")
  tidy_dat_test_elem <- tidy_dat_test[, which(names(tidy_dat_test) %in% item_names)]
  #daily data
  for (i in seq_along(item_names)){
    #build temporary dataframe exluding all other sale items
    temp_items <- item_names[!item_names %in% item_names[i]]
    temp_df_train <- tidy_dat_train[, -which(names(tidy_dat_train) %in% temp_items)]
    temp_df_test <- tidy_dat_test[, -which(names(tidy_dat_train) %in% temp_items)]
    model_daily <- lm(formula(form[i]), data = temp_df_train)
    forecast <- predict(model_daily, newdata = temp_df_test)
    res <- residuals(model_daily)
    mase_daily <- mase(pull(tidy_dat_test_elem[,i]),forecast)
    lb_daily <- Box.test(res, lag=10, type = "Lj")$p.value
    AIC_daily <- AIC(model_daily)
    #storing metrics
    test_mase <- c(test_mase, mase_daily)
    Ljung_Box_p_value <- c(Ljung_Box_p_value, lb_daily)
    Akaike_info_crit <- c(Akaike_info_crit, AIC_daily)
    seasonality <- c(seasonality, "NA")
  }
  output <- tibble(product = product, category = category,method = method, seasonality = seasonality, test_mase = test_mase, Ljung_Box_p_value = Ljung_Box_p_value, Akaike_info_crit = Akaike_info_crit)
  #return output
  print(output)
} 


#using regression
reg_output <- reg_model(tidy_dat_train, tidy_dat_test, item_names, category_names, train_xreg, test_xreg)



#using regression
reg_output <- reg_model(tidy_dat_train, tidy_dat_test, item_names, category_names, train_xreg, test_xreg)


############## REGRESSION W/ ARIMA ERRORS ##############



#building xreg
xreg_model <- function(train_series_daily, test_series_daily, train_series_weekly, test_series_weekly, item_names, category_names, train_xreg, test_xreg){
  #packages
  library(forecast)
  library(Metrics)
  #jitter test_xreg data to avoid rank deficiencies
  test_xreg <- as.matrix(as.data.frame(lapply(test_xreg, jitter)))
  train_xreg <- as.matrix(as.data.frame(lapply(train_xreg, jitter)))
  #initializing vectors
  product <- rep(item_names)
  category <- c(category_names)
  method <- rep("regression_arima_errors", length(train_series_daily))
  seasonality <- c()
  test_mase <- c()
  Ljung_Box_p_value <- c()
  Akaike_info_crit<- c()
  #daily data
  for (i in seq_along(train_series_weekly)){
    model_daily <- auto.arima(train_series_weekly[[i]], xreg= train_xreg)
    forecast_daily <- forecast(model_daily, h = 28, xreg = test_xreg)
    mase_daily <- mase(as.numeric(test_series_weekly[[i]]), as.numeric(forecast_daily$mean))
    res_daily <- residuals(forecast_daily)
    lb_daily <- Box.test(res_daily, lag=10, type = "Lj")$p.value
    AIC_daily <- AIC(model_daily)
    #storing metrics
    test_mase <- c(test_mase, mase_daily)
    Ljung_Box_p_value <- c(Ljung_Box_p_value, lb_daily)
    Akaike_info_crit <- c(Akaike_info_crit, AIC_daily)
    seasonality <- c(seasonality, "NA")
  }
  output <- tibble(product = product, category = category,method = method, seasonality = seasonality, test_mase = test_mase, Ljung_Box_p_value = Ljung_Box_p_value, Akaike_info_crit = Akaike_info_crit)
  #return output
  output
}

#using xreg
xreg_output <- xreg_model(train_series_daily, test_series_daily, train_series_weekly, test_series_weekly, item_names, category_names, train_xreg, test_xreg)

########################################
############## ALL MODELS ##############
########################################

#combining all models 
#all_functions_beals <- function(train_series_daily, test_series_daily, train_series_weekly, test_series_weekly, item_names, category_names, train_xreg, test_xreg){
  #each individual function
  avg_output <- avg_model(train_series_daily, test_series_daily, train_series_weekly, test_series_weekly, item_names, category_names, train_xreg, test_xreg)
  naive_output <- naive_model(train_series_daily, test_series_daily, train_series_weekly, test_series_weekly, item_names, category_names, train_xreg, test_xreg)
  snaive_output <- snaive_model(train_series_daily, test_series_daily, train_series_weekly, test_series_weekly, item_names, category_names, train_xreg, test_xreg)
  ARIMA_output <- ARIMA_model(train_series_daily, test_series_daily, train_series_weekly, test_series_weekly, item_names, category_names, train_xreg, test_xreg)
  SARIMA_output <- SARIMA_model(train_series_daily, test_series_daily, train_series_weekly, test_series_weekly, item_names, category_names, train_xreg, test_xreg)
  ARFIMA_output <- ARFIMA_model(train_series_daily, test_series_daily, train_series_weekly, test_series_weekly, item_names, category_names, train_xreg, test_xreg)
  reg_output <- reg_model(train_series_daily, test_series_daily, train_series_weekly, test_series_weekly, item_names, category_names, train_xreg, test_xreg)
  xreg_output <- xreg_model(train_series_daily, test_series_daily, train_series_weekly, test_series_weekly, item_names, category_names, train_xreg, test_xreg)
  #binding the results
  all_outputs <- rbind(avg_output, naive_output, snaive_output, ARIMA_output, SARIMA_output, ARFIMA_output, reg_output, xreg_output)
  #return all_outputs
  all_outputs
}

#result <- all_functions_beals(train_series_daily, test_series_daily, train_series_weekly, test_series_weekly, item_names, category_names, train_xreg, test_xreg)

final_output <- rbind(avg_output, naive_output, snaive_output, ARIMA_output, SARIMA_output, ARFIMA_output, reg_output)
write_csv(final_output,"/Users/breabeals/Documents/Classes/Q3/Time_Series/TS_Project/beals_output.csv")

