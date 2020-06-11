# runs all code from data_cleaning_functions file, creating the functions we need
source("project_code/data_cleaning_functions.R")

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




