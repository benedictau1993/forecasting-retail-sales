########################################################
############ BARK Regional Sales Prediction ############
############    UChicago TS | June, 2020    ############
########################################################

set.seed(123)
library(shiny)
library(shinyjs)
library(readxl)
library(tidyverse)
library(tseries)
library(xts)
library(tsbox)
library(DataCombine)
library(forecast)
library(plyr)
library(readr)
library(TSstudio)
library(fracdiff)
library(tsintermittent)
library(lmtest)
library(DescTools)
library(data.table)
library(fastDummies)
library(caret)
library(lubridate)
library(DAAG)
library(Metrics)
library(tictoc)
library(shiny)
library(nnfor)
library(DT)
options(stringsAsFactors = FALSE)
options(digits=3)

sales <- read.csv("sales_train_validation.csv")
cal <- read.csv("calendar.csv")
prices <- read.csv("sell_prices.csv")

shinyServer(function(input, output) {
    # Only enable controls after system has been initialised
    observeEvent(actu_avg_r,
      {
         enable("state")
         enable("category")
         enable("item_selector")
         enable("holdout_days")
         enable("window")
         enable("h")
         enable("t_naive")
         enable("t_arima")
         # enable("t_sarima")
         enable("t_arfima")
         enable("t_hw")
         enable("t_stl")
         enable("t_crost")
      }
    )
    output$item_selector <- renderUI({
        selectInput(inputId = "item", label = "Product item:", choices = eval(parse(text=(paste0("readRDS('", input$category, ".Rds')")))))
    })

    main_avg_r <- reactive({
        mydata <- sales %>% subset((state_id == input$state) & (item_id == input$item)) %>% select(store_id, d_1:d_1913)
        mydata_stores <- mydata$store_id
        mydata <- mydata %>% select(d_1:d_1913) %>% t %>% as.data.frame()
        colnames(mydata) <- mydata_stores
        mydata <- tibble::rownames_to_column(mydata, "d")
        mydata <- mydata %>% gather(store_id, sales, c(CA_1:CA_4), factor_key=TRUE)
        mydata$item_id <- input$item
        sales_df <- mydata
        # Read in calendar
        cal_df <- cal %>% select(date, wm_yr_wk, wday:snap_CA) 
        cal_df$event_name_1[cal_df$event_name_1 %like% "Mothers"] <- 'Mothers Day'
        # Merge calendar with sales in date code `d`
        main <- sales_df %>% left_join(cal_df, by="d")
        #Merge pricing info with previous on store_id, item_id, and week code. 
        main <- main %>% left_join(prices, by=c("store_id", "item_id", "wm_yr_wk"))
        main$sell_price[is.na(main$sell_price)] <- 0
        # Now we can group over store_id, sum over sales, and create new column avg_price, which denotes the average revenue (weighted average) of selling prices. 
        main_avg <- main %>% group_by(d, item_id) %>% dplyr::summarise(
            state_sales = sum(sales),
            avg_price = if (sum(sales)==0) mean(sell_price) else sum(sell_price * sales) / sum(sales),
            date = names(table(date))[which.max(table(date))],
            wm_yr_wk = names(table(wm_yr_wk))[which.max(table(wm_yr_wk))],
            wday = names(table(wday))[which.max(table(wday))],
            month = names(table(month))[which.max(table(month))],
            year = names(table(year))[which.max(table(year))],
            event_name_1 = names(table(event_name_1))[which.max(table(event_name_1))],
            event_type_1 = names(table(event_type_1))[which.max(table(event_type_1))],
            event_name_2 = names(table(event_name_2))[which.max(table(event_name_2))],
            event_type_2 = names(table(event_type_2))[which.max(table(event_type_2))],
            snap_CA = names(table(snap_CA))[which.max(table(snap_CA))],
        ) %>% ungroup() 
        cols <- (main_avg %>% colnames)[-c(3,4,5,7)]
        main_avg <- main_avg %>% mutate_at(cols, funs(factor(.)))
        main_avg$date <- as.Date(main_avg$date)
        main_avg$snap_CA <- as.numeric(main_avg$snap_CA)-1
        main_avg <- main_avg %>% dummy_cols(select_columns=c("wday", "month", "year", "event_name_1", "event_type_1", "event_name_2", "event_type_2"), remove_most_frequent_dummy=TRUE, remove_selected_columns=TRUE)
        main_avg
    })
    output$main_avg_r <- renderDataTable({as.data.table(main_avg_r())}, options=list(pageLength=5))
    
    # tsCV and holdout split
    item_data <- reactive( main_avg_r() %>% subset(item_id %in% input$item) %>% arrange(date) %>% head(-input$holdout_days)  )
    item_ts <- reactive( item_data() %>% ts(decimal_date(as.Date(item_data()$date[1])), frequency = 365.25) )
    state_sales <- reactive( item_ts()[,3] )
    day_of_week <- reactive( item_data()[, grepl("wday", names(item_data()))] %>% as.matrix )
    month_of_year <- reactive( item_data()[, grepl("month", names(item_data()))] %>% as.matrix )
    year_of_time <- reactive( item_data()[, grepl("year", names(item_data()))] %>% as.matrix )
    xreg_no_price <- reactive( item_data()[,c(7, 8:13, 54:56)] %>% as.matrix  )
    all_xreg <- reactive( item_data()[,c(4, 7, 8:13, 54:56)] %>% as.matrix  )
    
    test_item_data <- reactive( main_avg_r() %>% subset(item_id %in% input$item) %>% arrange(date) %>% tail(input$holdout_days) )
    test_item_ts <- reactive( test_item_data() %>% ts(decimal_date(as.Date(item_data()$date[(length(item_data()$date)-input$window+1)])), frequency = 365.25) )
    test_state_sales <- reactive( test_item_ts()[,3] )
    test_day_of_week <- reactive( test_item_data()[, grepl("wday", names(test_item_data()))] %>% as.matrix )
    test_month_of_year <- reactive( test_item_data()[, grepl("month", names(test_item_data()))] %>% as.matrix )
    test_year_of_time <- reactive( test_item_data()[, grepl("year", names(test_item_data()))] %>% as.matrix )
    test_xreg_no_price <- reactive( test_item_data()[,c(7, 8:13, 54:56)] %>% as.matrix  )
    test_all_xreg <- reactive( test_item_data()[,c(4, 7, 8:13, 54:56)] %>% as.matrix )
    
    # MASE
    mase <- function(e){
        mases <- c()
        for (i in 0:(length(state_sales())-input$h-input$window)){
            mases[i] <- (1/input$h*sum(abs(e[(input$window+i),]))) / ((1/(input$window-1))*sum(abs(diff(state_sales()[(1+i):(input$window+i)]))))
        }
        return(mean(mases, na.rm=TRUE) %>% round(4))
    }
    mase_notcv <- function(train, test, forecast){
        naive <- stats::lag(train)
        insample_mae <- mean(abs(train - naive), na.rm = TRUE)
        error_outsample <- test - forecast
        ase <- error_outsample / insample_mae
        mean(abs(ase), na.rm = TRUE) %>% round(4)
    }
    
    # Naive methods
    m_avg <- function(x, h){forecast(mean(x, na.rm=TRUE), h=h)} 
    e_avg <- reactive( if (input$t_naive) { tsCV(state_sales(), m_avg, h=input$h, window=input$window) %>% mase } else NA )
    e_naive <- reactive( if (input$t_naive) { tsCV(state_sales(), naive, h=input$h, window=input$window) %>% mase } else NA )
    e_snaive <- reactive( if (input$t_naive) { tsCV(state_sales(), snaive, h=input$h, window=input$window) %>% mase } else NA )
    e_rwf <- reactive( if (input$t_naive) { tsCV(state_sales(), rwf, h=input$h, window=input$window) %>% mase } else NA )
    e_ols <- reactive( NA )
    # Disabled OLS
    # e_ols <- lm(state_sales() ~ ., data=item_data()[ ,c(3, 4, 7, 8:13, 54:56)]) %>% 
    #   predict(newdata = test_item_data()[ ,c(3, 4, 7, 8:13, 54:56)]) %>%
    #   (Metrics::mase)(as.numeric(test_state_sales()))
    
    # Force t_naive checkbox always TRUE
    observeEvent(input$t_naive, shinyjs::disable("t_naive"))
    
    # Non-seasonal ARIMAs
    m_arima <- function(x, h){forecast(auto.arima(x, stationary=FALSE, seasonal=FALSE), h=h)}
    e_arima <- reactive( if (input$t_arima) { tsCV(state_sales(), m_arima, h=input$h, window=input$window) %>% mase } else NA )
    
    m_arima_day <- function(y, h, xreg){
        X <- xreg[1:length(y),]
        if(NROW(xreg) < length(y) + h)
            stop("Not enough xreg data for forecasting")
        newX <- xreg[length(y)+(1:h), ]
        fit <- auto.arima(y, stationary=FALSE, seasonal=FALSE, xreg=X)
        forecast(fit, xreg=newX)
    }
    e_arima_day <- reactive( if (input$t_arima) { tsCV(state_sales(), m_arima_day, h=input$h, window=input$window, xreg=day_of_week()) %>% mase } else NA )
    
    m_arima_xreg <- function(y, h, xreg){
        X <- xreg[1:length(y),]
        if(NROW(xreg) < length(y) + h)
            stop("Not enough xreg data for forecasting")
        newX <- xreg[length(y)+(1:h), ]
        fit <- auto.arima(y, stationary=FALSE, seasonal=FALSE, xreg=X)
        forecast(fit, xreg=newX)
    }
    e_arima_xreg <- reactive( if (input$t_arima) { tsCV(state_sales(), m_arima_xreg, h=input$h, window=input$window, xreg=xreg_no_price()) %>% mase } else NA )

    # Seasonal ARIMAs (slaw)
    m_sarima <- function(x, h){forecast(auto.arima(x, stationary=FALSE, seasonal=TRUE), h=h)}
    e_sarima <- reactive( if (input$t_sarima) { tsCV(ts(state_sales(), decimal_date(as.Date(item_data()$date[1])), frequency = 7), m_sarima, h=input$h, window=input$window) %>% mase } else NA )
    
    m_sarima_day <- function(y, h, xreg){
        X <- xreg[1:length(y),]
        if(NROW(xreg) < length(y) + h)
            stop("Not enough xreg data for forecasting")
        newX <- xreg[length(y)+(1:h), ]
        fit <- auto.arima(y, stationary=FALSE, seasonal=TRUE, xreg=X)
        forecast(fit, xreg=newX)
    }
    e_sarima_day <- reactive( if (input$t_sarima) { tsCV(ts(state_sales(), decimal_date(as.Date(item_data()$date[1])), frequency = 7), m_sarima_day, h=input$h, window=input$window, xreg=day_of_week()) %>% mase } else NA )
    
    m_sarima_xreg <- function(y, h, xreg){
        X <- xreg[1:length(y),]
        if(NROW(xreg) < length(y) + h)
            stop("Not enough xreg data for forecasting")
        newX <- xreg[length(y)+(1:h), ]
        fit <- auto.arima(y, stationary=FALSE, seasonal=TRUE, xreg=X)
        forecast(fit, xreg=newX)
    }
    e_sarima_xreg <- reactive( if (input$t_sarima) { tsCV(ts(state_sales(), decimal_date(as.Date(item_data()$date[1])), frequency = 7), m_sarima_xreg, h=input$h, window=input$window, xreg=xreg_no_price()) %>% mase } else NA )

    # ARFIMA
    m_arfima <- function(x, h){forecast(forecast::arfima(x), h=h)}
    e_arfima <- reactive( if (input$t_arfima) { tsCV(state_sales(), m_arfima, h=input$h, window=input$window) %>% mase } else NA )
    
    # # Holt Winters (additive only; state_sales() contains zeros)
    e_hw_add <- reactive( if (input$t_hw) { tsCV(ts(state_sales(), decimal_date(as.Date(item_data()$date[1])), frequency = 7), hw, h=input$h, window=input$window) %>% mase } else NA )

    # STL week and month seasonality
    m_stl_wk <- function(x, h){forecast(stl(x, s.window="periodic", robust=TRUE), h=h)}
    e_stl_wk <- reactive( if (input$t_stl) { tsCV(ts(state_sales(), decimal_date(as.Date(item_data()$date[1])), frequency = 7), m_stl_wk, h=input$h, window=input$window) %>% mase } else NA )
    m_stl_mo <- function(x, h){forecast(stl(x, s.window="periodic", robust=TRUE), h=h)}
    e_stl_mo <- reactive( if (input$t_stl) { tsCV(ts(state_sales(), decimal_date(as.Date(item_data()$date[1])), frequency = 30), m_stl_mo, h=input$h, window=input$window) %>% mase } else NA )
    
    # Croston (non-CV)
    # e_croston <- tsCV(state_sales(), croston, h=input$h, window=input$window) %>% mase
    e_croston <- reactive( if (input$t_crost) { 
        mase_notcv(
            head(state_sales(), -input$h),
            tail(state_sales(), input$h), 
            crost(state_sales(), h=input$h, init="mean", type="sbj", cost="mse")$component$c.out[1])} else NA )
    
    ### ### ### ### more models ### ### ### ### 
    
    # Gather results into df
    method_names <- c("Average", "Naive", "Seasonal Naive", "Random Walk Forests", "Least Squares", "ARIMA", "ARIMA w/day effect", "LS with ARIMA Errors", "SARIMA", "SARIMA w/day effect", "LS with SARIMA Errors", "Fractional ARIMA", "Holt-Winters", "STL Loess (week)", "STL Loess (month)", "Croston's Method (SBJ)")
    methods <- c("mean(na.rm=TRUE) %>% forecast(h=input$holdout_days)",
                 "naive(h=input$holdout_days)", 
                 "snaive(h=input$holdout_days)",
                 "rwf(h=input$holdout_days)",
                 "lm(state_sales() ~ ., data=item_data()[ ,c(3, 4, 7, 8:13, 54:56)]) %>% predict(newdata = test_item_data()[ ,c(3, 4, 7, 8:13, 54:56)]) ",
                 "auto.arima(stationary=FALSE, seasonal=FALSE) %>% forecast(h=input$holdout_days)",
                 "auto.arima(stationary=FALSE, seasonal=FALSE, xreg=day_of_week()) %>% forecast(h=input$holdout_days, xreg=test_day_of_week())",
                 "auto.arima(stationary=FALSE, seasonal=FALSE, xreg=xreg_no_price()) %>% forecast(h=input$holdout_days, xreg=test_xreg_no_price())",
                 "ts(start = decimal_date(as.Date(item_data()$date[1])), frequency = 7) %>% auto.arima(stationary=FALSE, seasonal=TRUE) %>% forecast(h=input$holdout_days)",
                 "ts(start = decimal_date(as.Date(item_data()$date[1])), frequency = 7) %>% auto.arima(stationary=FALSE, seasonal=TRUE, xreg=day_of_week()) %>% forecast(h=input$holdout_days, xreg=test_day_of_week())",
                 "ts(start = decimal_date(as.Date(item_data()$date[1])), frequency = 7) %>% auto.arima(stationary=FALSE, seasonal=TRUE, xreg=xreg_no_price()) %>% forecast(h=input$holdout_days, xreg=test_xreg_no_price())",  
                 "(forecast::arfima) %>% forecast(h=input$holdout_days)",
                 "ts(start = decimal_date(as.Date(item_data()$date[1])), frequency = 7) %>% hw(h=input$holdout_days)",
                 "ts(start = decimal_date(as.Date(item_data()$date[1])), frequency = 7) %>% stl(s.window='periodic', robust=TRUE) %>% forecast(h=input$holdout_days)",
                 "ts(start = decimal_date(as.Date(item_data()$date[1])), frequency = 30) %>% stl(s.window='periodic', robust=TRUE) %>% forecast(h=input$holdout_days)",
                 "croston(h=input$holdout_days)"
    )
    
    # Data table with models and MASE
    results_df_r <- reactive ({
        mase_results <- c(e_avg(), e_naive(), e_snaive(), e_rwf(), e_ols(), e_arima(), e_arima_day(), e_arima_xreg(), e_sarima(), e_sarima_day(), e_sarima_xreg(), e_arfima(), e_hw_add(), e_stl_wk(), e_stl_mo(), e_croston())
        data.frame(Model=method_names, MASE=mase_results, methods=methods)
    })
    output$results_df <- renderDataTable({
        as.data.table(results_df_r()[order(results_df_r()[,2]), c(1,2)], options=list(pageLength=5))
    })
    
    # Predict holdout
    predict_r <- reactive( eval(parse(text=(paste0( 'state_sales() %>%', methods[which.min(results_df_r()$MASE)]) ))) )
    
    # Plot
    plot_data_r <- reactive( 
        (main_avg_r() %>% subset(item_id %in% input$item) %>% arrange(date) %>% tail(input$window))$state_sales %>% 
            ts(decimal_date(as.Date(item_data()$date[(length(item_data()$date)-input$window+1)])), frequency = 365.25) %>% 
            cbind(predict_r()$mean %>% ts(decimal_date(as.Date(item_data()$date[(length(item_data()$date)-input$holdout_days+1)])), frequency = 365.25))
    )
    plot_r <- reactive( plot_data_r() %>% plot(plot.type="s", xlab="Date", ylab = "Daily sales in state", col=c("darkblue", "red"), lwd=c(1,3)) )
    output$plot_r <- renderPlot(plot_r())

    # historic average sale per day in the state
    hist_avg_r <- reactive( ( state_sales() %>% mean ) )
    output$hist_avg_r <- renderPrint( cat("Historic daily sales average:", format(hist_avg_r())) )
    # predicted average sale per week in the state
    pred_avg_r <- reactive( ( predict_r()$mean %>% mean ) )
    output$pred_avg_r <- renderPrint( cat("Predicted daily sales average:", format(pred_avg_r())) )
    # actual average sale per week in the state
    actu_avg_r <- reactive( (state_sales() %>% c(test_state_sales()) %>% mean) )
    output$actu_avg_r <- renderPrint( cat("Actual daily sales average:", format(actu_avg_r())) )
})

