library(Metrics)
library(fGarch)
library(hts)
library(forecast)

##########################################################################
# ARMA-GARCH
##########################################################################

arma_garch <- function(train_series, test_series, item_names, category_names) {
  # check frequency of one of the series to determine if series supplied is weekly or daily seasonality
  freq <- ifelse(frequency(train_series[[1]]) == 7, "weekly", "daily")
  # vectors to store results
  product <- c()
  category <- c()
  method <- c()
  seasonality <- rep(freq, times = length(item_names))
  test_mase <- c()
  Akaike_info_crit <- c()
  Ljung_Box_p_value <- c()
  
  for (i in seq_along(item_names)) {
    if (i != 1){print(paste("Fit and predicted for", item_names[i]))}
    # extract ARMA specs to use
    arma_order <- as.vector(arimaorder(auto.arima(train_series[[i]], seasonal = FALSE, d = 0)))
    ar_order <- as.numeric(arma_order[1])
    ar_order <- ifelse(ar_order > 2, 2, ar_order)
    ma_order <- as.numeric(arma_order[3])
    ma_order <- ifelse(ma_order > 2, 2, ma_order)
    arma_order <- paste0("(", ar_order, ", ", ma_order, ")")
    stored_formula <- paste0("formula = ~ arma(", ar_order, ",", ma_order, ") + garch(1, 1)")
    # try fitting ARMA-GARCH model if it fails log the time series and fit it there
    tryCatch({
      # fit and predict
      model <- garchFit(formula = formula(stored_formula), data = train_series[[i]], trace = FALSE)
      pred <- predict(model, n.ahead = 28)$meanForecast
      # record results
      product <- c(product, item_names[i])
      category <- c(category, category_names[i])
      method <- c(method, paste0("ARMA", arma_order, "-", "GARCH(1, 1)"))
      test_mase <- c(test_mase, mase(test_series[[i]], pred))
      Akaike_info_crit <- c(Akaike_info_crit, "NA")
      Ljung_Box_p_value <- c(Ljung_Box_p_value, Box.test(model@residuals, lag = ifelse(freq == "weekly", 14, 10), type = "Ljung-Box")$p.value)
    }, error = function(err) {
      # if previous fitting procedure produced an error, we try jittering the time series and differencing and trying again
      dat = diff(jitter(train_series[[i]]))
      model <- garchFit(formula = formula(stored_formula), data = dat, trace = FALSE)
      pred <- predict(model, n.ahead = 28, method = "ML")$meanForecast
      pred <- suppressWarnings(diffinv(pred, xi = tail(dat, 1)))
      pred <- ts(tail(pred, 28), frequency = ifelse(freq == "weekly", 7, 1), start = c(13, 2))
      # record results
      product <<- c(product, item_names[i])
      category <<- c(category, category_names[i])
      method <<- c(method, paste0("ARMA", arma_order, "-", "GARCH(1, 1)"))
      test_mase <<- c(test_mase, mase(test_series[[i]], pred))
      Akaike_info_crit <<- c(Akaike_info_crit, "NA")
      Ljung_Box_p_value <<- c(Ljung_Box_p_value, Box.test(model@residuals, lag = ifelse(freq == "weekly", 14, 10), type = "Ljung-Box")$p.value)
    }  
    )
  }
  print(paste("Fit and predicted for", item_names[i]))
  
  tibble(product = product, category = category, method = method, seasonality = seasonality, test_mase = test_mase, Akaike_info_crit = Akaike_info_crit,
         Ljung_Box_p_value = Ljung_Box_p_value)
  
}


##########################################################################
# NEURAL NET
##########################################################################

neural_net <- function(train_series, test_series, item_names, category_names) {
  # check frequency of one of the series to determine if series supplied is weekly or daily seasonality
  freq <- ifelse(frequency(train_series[[1]]) == 7, "weekly", "daily")
  # vectors to store results
  product <- c()
  category <- c()
  method <- c()
  seasonality <- rep(freq, times = length(item_names))
  test_mase <- c()
  Akaike_info_crit <- c()
  Ljung_Box_p_value <- c()
  
  for (i in seq_along(item_names)) {
    if (i != 1) {print(paste("Fit and predicted for", item_names[i]))}
    
    # fit and predict
    model <- nnetar(train_series[[i]])
    if (freq == "daily") {
      pred <- as.numeric(ts(forecast(model, h = 28)$mean, start = c(13, 2), frequency = ifelse(freq == "weekly", 7, 1)))
      actuals <- as.numeric(test_series[[i]])
      resids <- as.numeric(model$residuals)
    } else if (freq == "weekly") {
      pred <- ts(forecast(model, h = 28)$mean, start = c(13, 2), frequency = ifelse(freq == "weekly", 7, 1))
      actuals <- test_series[[i]]
      resids <- model$residuals
    }
    # record results
    p <- model$p
    P <- model$P
    k <- model$size
    m <- model$m
    spec <- paste0("NNAR(", p, ", ", P, ", ", k, ")[", m, "]")
    product <- c(product, item_names[i])
    category <- c(category, category_names[i])
    method <- c(method, spec)
    test_mase <- c(test_mase, mase(actuals, pred))
    Akaike_info_crit <- c(Akaike_info_crit, "NA")
    Ljung_Box_p_value <- c(Ljung_Box_p_value, Box.test(resids, lag = ifelse(freq == "weekly", 14, 10), type = "Ljung-Box")$p.value)
  }
  print(paste("Fit and predicted for", item_names[i]))
  
  tibble(product = product, category = category, method = method, seasonality = seasonality, test_mase = test_mase, Akaike_info_crit = Akaike_info_crit,
         Ljung_Box_p_value = Ljung_Box_p_value)
  
}


##########################################################################
# Hierarchical models
##########################################################################

hierarchical <- function(train_series, test_series, item_names, category_names, model_choice = "arima", reconcile = "top-down") {
  # check frequency of one of the series to determine if series supplied is weekly or daily seasonality
  freq <- ifelse(frequency(train_series[[1]]) == 7, "weekly", "daily")
  # vectors to store results
  product <- c()
  category <- c()
  method <- c()
  seasonality <- rep(freq, times = length(item_names))
  test_mase <- c()
  Akaike_info_crit <- c()
  Ljung_Box_p_value <- c()
  
  # function to create new item names that play nice with hts
  # works by front-padding hobbies and foods item names with "pa" or "pads" so they have the same length as household items
  hierarchy_name <- function(item_id) {
    if (nchar(item_id) == 15) {
      return(item_id)
    } else if (nchar(item_id) == 11) {
      return(paste0("pads", item_id))
    } else if (nchar(item_id) == 13) {
      return(paste0("pa", item_id))
    }
  }
  # function to reverse hierarchy name
  reverse_hierarchy_name <- function(item_id) {
    if (substr(item_id, 1, 3) == "pad") {
      return(substr(item_id, 5, 15))
    } else if (substr(item_id, 1, 3) == "paH") {
      return(substr(item_id, 3, 15))
    } else {
      return(item_id)
    }
  }
  
  # get new hierarchy names
  h_item_names <- as.character(sapply(item_names, hierarchy_name))
  # create ts matrix to pass to hts
  text_to_parse <- paste0("train_series_weekly[[", 1:length(train_series), "]]", collapse = ", ")
  text_to_parse <- paste0("cbind(", text_to_parse, ")")
  ts_matrix <- eval(parse(text = text_to_parse))
  dimnames(ts_matrix)[[2]] <- h_item_names
  
  # create hts
  hts_object <- hts(ts_matrix, characters = c(11, 4))
  # determine how we're going to forecast
  approach <- case_when(
    ((reconcile == "top-down") & (model_choice == "arima")) ~ "tdgsa",
    ((reconcile == "middle-out") & (model_choice == "arima")) ~ "flagged",
    ((reconcile == "middle-out") & (model_choice == "ets")) ~ "mo",
    ((reconcile == "top-down") & (model_choice == "ets")) ~ "tdfp",
    ((reconcile == "top-down") & (model_choice == "rw")) ~ "tdfp"
  )
  
  # compute forecast depending on method
  if (approach == "flagged") {
    stop("implementation for middle-out approach using ARIMA not built out yet. Sorry!")
    
  } else if (reconcile == "top-down") {
    # compute forecast
    hts_forecast <- forecast(hts_object, method = approach, fmethod = model_choice, h = 28, keep.resid = TRUE)
    forecast_names <- dimnames(hts_forecast$bts)[[2]]
    
    for (i in seq_along(forecast_names)) {
      if (i != 1) {print(paste("Fit and predicted for", reverse_hierarchy_name(forecast_names[i])))}
      
      if (freq == "daily"){
        pred <- as.numeric(ts((as.numeric(hts_forecast$bts[,i])), start = c(13, 2), frequency = ifelse(freq == "weekly", 7, 1)))
        ind <- match(reverse_hierarchy_name(forecast_names[i]), item_names)
        actuals <- test_series[[ind]]
        actuals <- as.numeric(actuals)
        resids <- as.numeric(hts_forecast$residuals[, i])
      } else if (freq == "weekly"){
        pred <- ts((as.numeric(hts_forecast$bts[,i])), start = c(13, 2), frequency = ifelse(freq == "weekly", 7, 1))
        ind <- match(reverse_hierarchy_name(forecast_names[i]), item_names)
        actuals <- test_series[[ind]]
        resids <- hts_forecast$residuals[, i]
      }
      product <- c(product, reverse_hierarchy_name(forecast_names[i]))
      category_id <- case_when(substr(forecast_names[i], 1, 3) == "pad" ~ substr(forecast_names[i], 5, 11),
                               substr(forecast_names[i], 1, 3) == "paH" ~ substr(forecast_names[i], 3, 11),
                               TRUE ~ substr(forecast_names[i], 1, 11))
      category <- c(category, category_id)
      approach_name <- paste0("hierarchical ", "(", reconcile, ")", "(", model_choice, ")")
      method <- c(method, approach_name)
      test_mase <- c(test_mase, mase(actuals, pred))
      Akaike_info_crit <- c(Akaike_info_crit, "NA")
      Ljung_Box_p_value <- c(Ljung_Box_p_value, Box.test(resids, lag = ifelse(freq == "weekly", 14, 10), type = "Ljung-Box")$p.value)
    }
    
    
  } else if (reconcile == "middle-out") {
    # compute forecast
    hts_forecast <- forecast(hts_object, method = "mo", fmethod = "ets", h = 28, keep.resid = TRUE, level = 1)
    forecast_names <- dimnames(hts_forecast$bts)[[2]]
    
    for (i in seq_along(forecast_names)) {
      if (i != 1) {print(paste("Fit and predicted for", reverse_hierarchy_name(forecast_names[i])))}
      
      if (freq == "daily"){
        pred <- as.numeric(ts((as.numeric(hts_forecast$bts[,i])), start = c(13, 2), frequency = ifelse(freq == "weekly", 7, 1)))
        ind <- match(reverse_hierarchy_name(forecast_names[i]), item_names)
        actuals <- test_series[[ind]]
        actuals <- as.numeric(actuals)
        resids <- as.numeric(hts_forecast$residuals[, i])
      } else if (freq == "weekly"){
        pred <- ts((as.numeric(hts_forecast$bts[,i])), start = c(13, 2), frequency = ifelse(freq == "weekly", 7, 1))
        ind <- match(reverse_hierarchy_name(forecast_names[i]), item_names)
        actuals <- test_series[[ind]]
        resids <- hts_forecast$residuals[, i]
      }
      product <- c(product, reverse_hierarchy_name(forecast_names[i]))
      category_id <- case_when(substr(forecast_names[i], 1, 3) == "pad" ~ substr(forecast_names[i], 5, 11),
                               substr(forecast_names[i], 1, 3) == "paH" ~ substr(forecast_names[i], 3, 11),
                               TRUE ~ substr(forecast_names[i], 1, 11))
      category <- c(category, category_id)
      approach_name <- paste0("hierarchical ", "(", reconcile, ")", "(", model_choice, ")")
      method <- c(method, approach_name)
      test_mase <- c(test_mase, mase(actuals, pred))
      Akaike_info_crit <- c(Akaike_info_crit, "NA")
      Ljung_Box_p_value <- c(Ljung_Box_p_value, Box.test(resids, lag = ifelse(freq == "weekly", 14, 10), type = "Ljung-Box")$p.value)
    }
  }
  
  print(paste("Fit and predicted for", reverse_hierarchy_name(forecast_names[i])))
  
  tibble(product = product, category = category, method = method, seasonality = seasonality, test_mase = test_mase, Akaike_info_crit = Akaike_info_crit,
         Ljung_Box_p_value = Ljung_Box_p_value)
  
}


