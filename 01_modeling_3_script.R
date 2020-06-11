source("project_code/yannik_modeling_functions.R")

# ARMA-GARCH models
daily_arma_garch_results <- arma_garch(train_series_daily, test_series_daily, item_names, category_names)
weekly_arma_garch_results <- arma_garch(train_series_weekly, test_series_weekly, item_names, category_names)
arma_garch_results <- bind_rows(daily_arma_garch_results, weekly_arma_garch_results)
# write_csv(arma_garch_results, "m5_data/arma_garch_results.csv")

# neural nets
daily_neural_net_results <- neural_net(train_series_daily, test_series_daily, item_names, category_names)
weekly_neural_net_results <- neural_net(train_series_weekly, test_series_weekly, item_names, category_names)
neural_net_results <- bind_rows(daily_neural_net_results, weekly_neural_net_results)
# write_csv(neural_net_results, "m5_data/neural_net_results.csv")

# hierarchical models
h_td_ets_daily_results <- hierarchical(train_series_daily, test_series_daily, item_names, category_names, 
                                       model_choice = "ets", reconcile = "top-down")
h_td_ets_weekly_results <- hierarchical(train_series_weekly, test_series_weekly, item_names, category_names, 
                                        model_choice = "ets", reconcile = "top-down")
h_mo_ets_daily_results <- hierarchical(train_series_daily, test_series_daily, item_names, category_names, 
                                       model_choice = "ets", reconcile = "middle-out")
h_mo_ets_weekly_results <- hierarchical(train_series_weekly, test_series_weekly, item_names, category_names, 
                                        model_choice = "ets", reconcile = "middle-out")
h_td_arima_daily_results <- hierarchical(train_series_daily, test_series_daily, item_names, category_names, 
                                         model_choice = "arima", reconcile = "top-down")
h_td_arima_weekly_results <- hierarchical(train_series_weekly, test_series_weekly, item_names, category_names, 
                                          model_choice = "arima", reconcile = "top-down")
hierarchical_results <- bind_rows(h_td_arima_daily_results, h_td_arima_weekly_results, h_td_ets_daily_results, h_td_ets_weekly_results,
                                  h_mo_ets_daily_results, h_td_ets_weekly_results)
# write_csv(hierarchical_results, "m5_data/hierarchical_results.csv")

# final result
results <- bind_rows(arma_garch_results, neural_net_results, hierarchical_results)
write_csv(results, "m5_data/yannik_modeling_results.csv")
