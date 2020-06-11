library(tidyverse)
# get a character vector of dept_ids
# takes a vector of item_ids and returns a vector of dept_ids of same length
generate_dept_ids <- function(item_ids) {
  # extract dept id from an item_id string
  extract_dept_id <- function(item_id) {
    substr(item_id, start = 1, stop = nchar(item_id) - 4)
  }
  # apply above function to vector of item_ids
  as.vector(apply(as.matrix(item_ids), MARGIN = 1, FUN = extract_dept_id))
}  



##########################################################################
# tidy_up()
##########################################################################

# get tidy data
# takes a vector of item_ids, state_ids, M5 sales training data, price data and calendar data
# and returns a tibble with sales data for the states specified aggregated/grouped by item_id, 
# with each item's time series stored as a column vector, and categorical variables of interest dummy-coded
tidy_up <- function(item_ids, state_ids, sales_dat, calendar_dat, prices_dat) {
  # filter sales and price data for items and states we want
  sales_dat <- sales_dat %>% filter(item_id %in% item_ids & state_id %in% state_ids)
  prices_dat <- prices_dat %>% filter(item_id %in% item_ids & substr(store_id,1,2) %in% state_ids) 
  
  # filter calendar for our date range
  calendar_dat <- calendar_dat[1:1913,]
  
  # check for any NAs outside of event variables, if any detected output warning
  if (any(is.na(calendar_dat %>% select(-(event_name_1:event_type_2)))) | 
      any(is.na(sales_dat)) | any(is.na(prices_dat))
      == TRUE) {warning("NAs detected. Recommended to impute or drop NA values before analysis")}
  
  # group by product in sales data, summing up sales
  suppressWarnings(sales_dat <- sales_dat %>% 
                     group_by(item_id) %>%
                     summarize_at(vars(d_1:d_1913), funs(sum)))
  
  # group by product in price data, averaging price
  prices_dat <- prices_dat %>% group_by(item_id, wm_yr_wk) %>% 
    summarize(sell_price = mean(sell_price))
  
  # transpose sales data so each series stored in a column
  sales_dat_copy <- sales_dat
  sales_dat <- t(as.data.frame(sales_dat_copy[,-1]))
  suppressWarnings(sales_dat <- as_tibble(sales_dat))
  colnames(sales_dat) <- item_ids
  
  # bind sales and calendar
  sales_dat <- bind_cols(sales_dat, calendar_dat)
  
  # extract item price series from sell_prices and join with sales data
  for (item in item_ids) {
    # to use for naming
    temp <- paste0('price_', item)
    
    # extract a product's price series
    item_price <- prices_dat %>% 
      filter(item_id == item) %>%
      ungroup() %>%
      select(wm_yr_wk, sell_price) %>%
      rename(!!temp := sell_price)
    
    # join with sales data
    sales_dat <- left_join(sales_dat, item_price, by = "wm_yr_wk")
  }
  
  # dummy code weekdays, with Saturday as the reference class
  sales_dat <- sales_dat %>% pivot_wider(names_from = weekday, values_from = wday)
  dummy_code <- function(x, na.rm = FALSE) ifelse(is.na(x), 0, 1)
  sales_dat <- sales_dat %>% mutate_at(vars(Saturday:Friday), funs(dummy_code))
  sales_dat <- sales_dat %>% select(-(Saturday))
  message("Week days dummy-coded with Saturday as the reference class.")
  
  # dummy code months, with January as the reference class
  months <- c('February','March','April','May','June','July','August','September','October','November','December')
  for (i in 1:length(months)) {
    month_dummy <- as.numeric(sales_dat$month == i+1)
    sales_dat[months[i]] <- month_dummy
  }
  message("Months dummy-coded with January as the reference class.")
  
  # dummy code event types
  sales_dat <- sales_dat %>% mutate(Sporting = if_else(event_type_1 == 'Sporting' | event_type_2 == 'Sporting',1,0, missing = 0),
                                    Cultural = if_else(event_type_1 == 'Cultural' | event_type_2 == 'Cultural',1,0, missing = 0),
                                    National = if_else(event_type_1 == 'National' | event_type_2 == 'National',1,0, missing = 0),
                                    Religious = if_else(event_type_1 == 'Religious' | event_type_2 == 'Religious',1,0, missing = 0))
  message("Event types dummy-coded.")
  
  # create trend variable
  sales_dat$trend <- seq(1, nrow(sales_dat))
  message("Created trend variable.")
  
  # drop variables we've created dummies for and other variables we don't care for
  snaps <- paste0("snap_", state_ids)
  no_snaps <- c("snap_TX", "snap_WI", "snap_CA")[!(c("snap_TX", "snap_WI", "snap_CA") %in% snaps)]
  sales_dat <- sales_dat %>% select(-d,-(all_of(no_snaps)), -month, -(event_name_1:event_type_2))
  message("Dropped snap variables for states not specified.")
  message("Dropped event name column and event types columns (since they've been dummied).")
  
  # reorder for aesthetics
  sales_dat <- sales_dat %>% select(trend, date, year, (all_of(item_ids)),
                                    (paste0('price_',item_ids[1]):paste0('price_',item_ids[length(items)])),
                                    (paste0('snap_',state_ids[1]):paste0('snap_',state_ids[length(states)])),
                                    (Sporting:Religious), (Sunday:Friday), (February:December))
  
  # return sales data
  sales_dat
}


##########################################################################
# extract_train_weekly()
##########################################################################

# functions to get training and test data
# takes a vector of item_ids and tidy sales data
# returns a list, with each element being a sales time series object
# train are observations 1:1885, test are observations 1886:1913
# weekly frequency = 7, daily frequency = 1
# naming convention for each ts is "train/test_weekly/daily_ITEM_ID", 
# e.g. "train_weekly_FOODS_2_197" is the training series with weekly seasonality for item_id FOODS_2_197
# note: when indexing into these lists (while in a loop, for example), I recommend double square brackets, i.e. [[]]
# because list[[i]] returns the ts object itself, while list[i] returns the list with only the ts object i inside
extract_train_weekly <- function(item_ids, tidy_sales_dat) {
  # create vector of names for time series
  series_names <- paste0("train_weekly_", item_ids)
  # start specification for ts object
  startW <- as.numeric(strftime(tidy_sales_dat$date[1], format = "%W"))
  startD <- as.numeric(strftime(tidy_sales_dat$date[1] + 1, format =" %w")) 
  
  # iterate along item_ids and extract training time series for each
  # assigning it to the names defined earlier
  for (i in seq_along(item_ids)) {
    assign(series_names[i],
           ts(eval(pull(tidy_sales_dat, item_ids[i])[1:1885]), frequency = 7, start = c(startW, startD)),
    )
    
  }
  # some parsing gymnastics to create a list of the variables created in the loop
  to_parse <- paste("list(", paste(eval(parse(text = "series_names")), collapse = ", "), ")")
  
  # assign series names to the list of time series
  return_list <- eval(parse(text = to_parse))
  names(return_list) <- series_names
  
  return_list
}

##########################################################################
# extract_train_daily()
##########################################################################

extract_train_daily <- function(item_ids, tidy_sales_dat) {
  # create vector of names for time series
  series_names <- paste0("train_daily_", item_ids)
  # start specification for ts object
  startD <- as.numeric(strftime(tidy_sales_dat$date[1], format = "%j"))
  
  # iterate along item_ids and extract training time series for each
  # assigning it to the names defined earlier
  for (i in seq_along(item_ids)) {
    assign(series_names[i],
           ts(eval(pull(tidy_sales_dat, item_ids[i])[1:1885]), frequency = 1, start = startD),
    )
    
  }
  # some parsing gymnastics to create a list of the variables created in the loop
  to_parse <- paste("list(", paste(eval(parse(text = "series_names")), collapse = ", "), ")")
  
  # assign series names to the list of time series
  return_list <- eval(parse(text = to_parse))
  names(return_list) <- series_names
  
  return_list
}

##########################################################################
# extract_test_weekly()
##########################################################################

extract_test_weekly <- function(item_ids, tidy_sales_dat) {
  # create vector of names for time series
  series_names <- paste0("test_weekly_", item_ids)
  # start specification for ts object
  startW <- as.numeric(strftime(tidy_sales_dat$date[1886], format = "%W"))
  startD <- as.numeric(strftime(tidy_sales_dat$date[1886] + 1, format =" %w")) 
  
  # iterate along item_ids and extract training time series for each
  # assigning it to the names defined earlier
  for (i in seq_along(item_ids)) {
    assign(series_names[i],
           ts(eval(pull(tidy_sales_dat, item_ids[i])[1886:1913]), frequency = 7, start = c(startW, startD)),
    )
    
  }
  # some parsing gymnastics to create a list of the variables created in the loop
  to_parse <- paste("list(", paste(eval(parse(text = "series_names")), collapse = ", "), ")")
  
  # assign series names to the list of time series
  return_list <- eval(parse(text = to_parse))
  names(return_list) <- series_names
  
  return_list
}

##########################################################################
# extract_test_daily()
##########################################################################

extract_test_daily <- function(item_ids, tidy_sales_dat) {
  # create vector of names for time series
  series_names <- paste0("test_daily_", item_ids)
  # start specification for ts object
  startD <- as.numeric(strftime(tidy_sales_dat$date[1886], format = "%j"))
  
  # iterate along item_ids and extract training time series for each
  # assigning it to the names defined earlier
  for (i in seq_along(item_ids)) {
    assign(series_names[i],
           ts(eval(pull(tidy_sales_dat, item_ids[i])[1886:1913]), frequency = 1, start = startD),
    )
    
  }
  # some parsing gymnastics to create a list of the variables created in the loop
  to_parse <- paste("list(", paste(eval(parse(text = "series_names")), collapse = ", "), ")")
  
  # assign series names to the list of time series
  return_list <- eval(parse(text = to_parse))
  names(return_list) <- series_names
  
  return_list
}

##########################################################################
# extract_train_xreg()
##########################################################################
# get training and test xreg matrices
# external regressors include: prices for all item_ids specified, event type dummies, snap dummies for state_ids specified
# takes a vector of item_ids, a vector of state_ids, and tidy sales data
# returns a matrix of external regressors
extract_train_xreg <- function(item_ids, state_ids, tidy_sales_dat) {
  snaps <- paste0("snap_", state_ids)
  prices <- paste0("price_", item_ids)
  
  return_mat <- tidy_sales_dat %>% select(all_of(prices), all_of(snaps), (Sporting:Religious))
  as.matrix(return_mat)[1:1885, -c(9, 10, 19, 22, 23, 24, 25)]
}

##########################################################################
# extract_test_xreg()
##########################################################################
extract_test_xreg <- function(item_ids, state_ids, tidy_sales_dat) {
  snaps <- paste0("snap_", state_ids)
  prices <- paste0("price_", item_ids)
  
  return_mat <- tidy_sales_dat %>% select(all_of(prices), all_of(snaps), (Sporting:Religious))
  as.matrix(return_mat)[1886:1913, -c(9, 10, 19, 22, 23, 24, 25)]
}

