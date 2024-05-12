# Check if 'pacman' package is installed; if not, install it.
if (!require("pacman")) {
  install.packages("pacman")
}

# Use 'pacman' package to load necessary packages for data manipulation, time series analysis, and visualization.
pacman::p_load(
  dplyr,       # For data manipulation
  fable,       # For forecasting models
  fabletools,  # Tools for working with 'fable' objects
  feasts,      # For time series features and statistics
  future,      # plan multisession
  lubridate,   # dates
  tidyr,       # For data tidying
  tsibble      # For time series data manipulation
)

options(scipen = 999)

# Load number of working days
working_days <- readRDS("data/working_days.RDS")

cleaned_data <- readRDS("data/cleaned_data_final.RDS") |> 
  select(-sales_amount, -qty_sold) |>  
  as_tsibble(key = c(state_id,
                     store_id,
                     cat_id,
                     dept_id),
             index = month,
             regular = TRUE) |>
  fill_gaps(daily_sales = 0,
            daily_qty = 0)   # Fill missing values with 0


# Aggregate sales data by category / department and state / store
agg_data <- cleaned_data |> 
  aggregate_key((cat_id / dept_id) * (state_id / store_id), daily_sales = sum(daily_sales), daily_qty = sum(daily_qty))

months_till_year_end <- lubridate::interval(max(cleaned_data$month |> as.Date()), ymd(paste0(year(max(cleaned_data$month |> as.Date())), "/12", "/01"))) / months(1)

# Sales FC ----
plan(multisession, workers = 3)

## Fit models on filtered data ----
fit_sales <- agg_data |>
  filter_index(~ "2015-11") |>  # Filter data up to DATE
  model(ETS_base = ETS(daily_sales),     # Exponential smoothing model
        ARIMA_base = ARIMA(daily_sales),   # ARIMA model
        Naive_base = NAIVE(daily_sales),
        Seasonal_naive_base = SNAIVE(daily_sales)) |> 
  reconcile(
    ETS_bu = bottom_up(ETS_base),    # Bottom-up reconciliation ETS base
    ETS_mint = min_trace(ETS_base, method = "wls_var"),  # MinT shrinkage reconciliation ETS base
    ETS_mint_ols = min_trace(ETS_base, method = "ols"),  # MinT shrinkage reconciliation ETS base
    ARIMA_bu = bottom_up(ARIMA_base),    # Bottom-up reconciliation ARIMA base
    ARIMA_mint = min_trace(ARIMA_base, method = "wls_var"),  # MinT shrinkage reconciliation ARIMA base
    ARIMA_mint_ols = min_trace(ARIMA_base, method = "ols"),  # MinT shrinkage reconciliation ARIMA base
    Naive_bu = bottom_up(Naive_base),    # Bottom-up reconciliation Naive base
    Naive_mint = min_trace(Naive_base, method = "wls_var"),  # MinT shrinkage reconciliation Naive base
    Naive_mint_ols = min_trace(Naive_base, method = "ols"),  # MinT shrinkage reconciliation Naive base
    Seasonal_naive_bu = bottom_up(Seasonal_naive_base),    # Bottom-up reconciliation Seasonal_naive base
    Seasonal_naive_mint = min_trace(Seasonal_naive_base, method = "wls_var"),  # MinT shrinkage reconciliation Seasonal_naive base
    Seasonal_naive_mint_ols = min_trace(Seasonal_naive_base, method = "ols")  # MinT shrinkage reconciliation Seasonal_naive base
  )

# Forecast 6 months
fc_sales <- fit_sales |> forecast(h = "6 months")

plan(sequential)

# Evaluate accuracy of models
accuracy_results_sales <- fc_sales |>
  accuracy(
    data = agg_data,
    measures = list(rmse = RMSE, mase = MASE, mape = MAPE)
  )

## Select best MODEL for sales amount FC and save FC ----

best_models_sales <- accuracy_results_sales |>
  filter(!is_aggregated(store_id), !is_aggregated(dept_id)) |> 
  group_by(store_id,
           dept_id)  |> 
  slice_min(mape)  |> 
  ungroup() |> 
  select(c(dept_id,
           store_id,
           mase,
           mape,
           rmse,
           .model)) |> 
  mutate(keep = TRUE)

best_models_sales <- best_models_sales |> 
  mutate(key = paste0(store_id, dept_id)) |> 
  select(key) |> 
  distinct() |> 
  mutate(store_id = substring(key, 1, 4),
         dept_id = substring(key, 5)) |> 
  select(-key) |> 
  left_join(best_models_sales |> select(-c(mase, mape, rmse)), join_by(dept_id == dept_id, store_id == store_id), multiple = "first", keep = FALSE) |> 
  distinct()

## Forecast future months ----

plan(multisession, workers = 6)

# Fit models on all data
fit_sales_all <- agg_data |>
  model(ETS_base = ETS(daily_sales),     # Exponential smoothing model
        ARIMA_base = ARIMA(daily_sales),   # ARIMA model
        Naive_base = NAIVE(daily_sales),
        Seasonal_naive_base = SNAIVE(daily_sales)) |> 
  reconcile(
    ETS_bu = bottom_up(ETS_base),    # Bottom-up reconciliation ETS base
    ETS_mint = min_trace(ETS_base, method = "wls_var"),  # MinT shrinkage reconciliation ETS base
    ETS_mint_ols = min_trace(ETS_base, method = "ols"),  # MinT shrinkage reconciliation ETS base
    ARIMA_bu = bottom_up(ARIMA_base),    # Bottom-up reconciliation ARIMA base
    ARIMA_mint = min_trace(ARIMA_base, method = "wls_var"),  # MinT shrinkage reconciliation ARIMA base
    ARIMA_mint_ols = min_trace(ARIMA_base, method = "ols"),  # MinT shrinkage reconciliation ARIMA base
    Naive_bu = bottom_up(Naive_base),    # Bottom-up reconciliation Naive base
    Naive_mint = min_trace(Naive_base, method = "wls_var"),  # MinT shrinkage reconciliation Naive base
    Naive_mint_ols = min_trace(Naive_base, method = "ols"),  # MinT shrinkage reconciliation Naive base
    Seasonal_naive_bu = bottom_up(Seasonal_naive_base),    # Bottom-up reconciliation Seasonal_naive base
    Seasonal_naive_mint = min_trace(Seasonal_naive_base, method = "wls_var"),  # MinT shrinkage reconciliation Seasonal_naive base
    Seasonal_naive_mint_ols = min_trace(Seasonal_naive_base, method = "ols")  # MinT shrinkage reconciliation Seasonal_naive base
  )

# Forecast X months
fc_sales_all <- fit_sales_all |> forecast(h = paste0(months_till_year_end, " months"))

plan(sequential)


# Qty FC ----
plan(multisession, workers = 6)

## Fit models on filtered data ----
fit_qty <- agg_data |>
  filter_index(~ "2015-11") |>  # Filter data up to DATE
  model(ETS_base = ETS(daily_qty),     # Exponential smoothing model
        ARIMA_base = ARIMA(daily_qty),   # ARIMA model
        Naive_base = NAIVE(daily_qty),
        Seasonal_naive_base = SNAIVE(daily_qty)) |> 
  reconcile(
    ETS_bu = bottom_up(ETS_base),    # Bottom-up reconciliation ETS base
    ETS_mint = min_trace(ETS_base, method = "wls_var"),  # MinT shrinkage reconciliation ETS base
    ETS_mint_ols = min_trace(ETS_base, method = "ols"),  # MinT shrinkage reconciliation ETS base
    ARIMA_bu = bottom_up(ARIMA_base),    # Bottom-up reconciliation ARIMA base
    ARIMA_mint = min_trace(ARIMA_base, method = "wls_var"),  # MinT shrinkage reconciliation ARIMA base
    ARIMA_mint_ols = min_trace(ARIMA_base, method = "ols"),  # MinT shrinkage reconciliation ARIMA base
    Naive_bu = bottom_up(Naive_base),    # Bottom-up reconciliation Naive base
    Naive_mint = min_trace(Naive_base, method = "wls_var"),  # MinT shrinkage reconciliation Naive base
    Naive_mint_ols = min_trace(Naive_base, method = "ols"),  # MinT shrinkage reconciliation Naive base
    Seasonal_naive_bu = bottom_up(Seasonal_naive_base),    # Bottom-up reconciliation Seasonal_naive base
    Seasonal_naive_mint = min_trace(Seasonal_naive_base, method = "wls_var"),  # MinT shrinkage reconciliation Seasonal_naive base
    Seasonal_naive_mint_ols = min_trace(Seasonal_naive_base, method = "ols")  # MinT shrinkage reconciliation Seasonal_naive base
  )

# Forecast 6 months
fc_qty <- fit_qty |> forecast(h = "6 months")

plan(sequential)


# Evaluate accuracy of models
accuracy_results_qty <- fc_qty |>
  accuracy(
    data = agg_data,
    measures = list(rmse = RMSE, mase = MASE, mape = MAPE)
  )

## Select best for qty FC and save FC ----

best_models_qty <- accuracy_results_qty |>
  filter(!is_aggregated(store_id), !is_aggregated(dept_id)) |>  
  group_by(store_id,
           dept_id)  |> 
  slice_min(mape)  |> 
  ungroup() |> 
  select(c(dept_id,
           store_id,
           mase,
           mape,
           rmse,
           .model)) |> 
  mutate(keep = TRUE)

best_models_qty <- best_models_qty |> 
  mutate(key = paste0(store_id, dept_id)) |> 
  select(key) |> 
  distinct() |> 
  mutate(store_id = substring(key, 1, 4),
         dept_id = substring(key, 5)) |> 
  select(-key) |> 
  left_join(best_models_qty |> select(-c(mase, mape, rmse)), join_by(dept_id == dept_id, store_id == store_id), multiple = "first", keep = FALSE) |> 
  distinct()

## Forecast future months ----

plan(multisession, workers = 6)

# Fit models on all data
fit_qty_all <- agg_data |>
  model(ETS_base = ETS(daily_qty),     # Exponential smoothing model
        ARIMA_base = ARIMA(daily_qty),   # ARIMA model
        Naive_base = NAIVE(daily_qty),
        Seasonal_naive_base = SNAIVE(daily_qty)) |> 
  reconcile(
    ETS_bu = bottom_up(ETS_base),    # Bottom-up reconciliation ETS base
    ETS_mint = min_trace(ETS_base, method = "wls_var"),  # MinT shrinkage reconciliation ETS base
    ETS_mint_ols = min_trace(ETS_base, method = "ols"),  # MinT shrinkage reconciliation ETS base
    ARIMA_bu = bottom_up(ARIMA_base),    # Bottom-up reconciliation ARIMA base
    ARIMA_mint = min_trace(ARIMA_base, method = "wls_var"),  # MinT shrinkage reconciliation ARIMA base
    ARIMA_mint_ols = min_trace(ARIMA_base, method = "ols"),  # MinT shrinkage reconciliation ARIMA base
    Naive_bu = bottom_up(Naive_base),    # Bottom-up reconciliation Naive base
    Naive_mint = min_trace(Naive_base, method = "wls_var"),  # MinT shrinkage reconciliation Naive base
    Naive_mint_ols = min_trace(Naive_base, method = "ols"),  # MinT shrinkage reconciliation Naive base
    Seasonal_naive_bu = bottom_up(Seasonal_naive_base),    # Bottom-up reconciliation Seasonal_naive base
    Seasonal_naive_mint = min_trace(Seasonal_naive_base, method = "wls_var"),  # MinT shrinkage reconciliation Seasonal_naive base
    Seasonal_naive_mint_ols = min_trace(Seasonal_naive_base, method = "ols")  # MinT shrinkage reconciliation Seasonal_naive base
  )

# Forecast X months
fc_qty_all <- fit_qty_all |> forecast(h = paste0(months_till_year_end, " months"))

plan(sequential)

# Write FC ----

fc_sales_best <- fc_sales_all |> 
  filter(!is_aggregated(dept_id), !is_aggregated(store_id), !is_aggregated(state_id), !is_aggregated(cat_id)) |> 
  left_join(best_models_sales, join_by(dept_id == dept_id,
                                       store_id == store_id,
                                       .model == .model), keep = FALSE) |>
  filter(keep == TRUE) |> 
  mutate(key_date = as.Date(month)) |> 
  mutate(conf_int_50 = hilo(daily_sales, 50),
         conf_int_60 = hilo(daily_sales, 60),
         conf_int_70 = hilo(daily_sales, 70),
         conf_int_80 = hilo(daily_sales, 80),
         conf_int_90 = hilo(daily_sales, 90)) |> 
  unpack_hilo("conf_int_50") |> 
  unpack_hilo("conf_int_60") |> 
  unpack_hilo("conf_int_70") |> 
  unpack_hilo("conf_int_80") |> 
  unpack_hilo("conf_int_90") |> 
  left_join(working_days, join_by(key_date == month), keep = FALSE) |> 
  mutate(sales_amount = .mean * days,
         sales_conf_int_50_lower = conf_int_50_lower * days,
         sales_conf_int_50_upper = conf_int_50_upper * days,
         sales_conf_int_60_lower = conf_int_60_lower * days,
         sales_conf_int_60_upper = conf_int_60_upper * days,
         sales_conf_int_70_lower = conf_int_70_lower * days,
         sales_conf_int_70_upper = conf_int_70_upper * days,
         sales_conf_int_80_lower = conf_int_80_lower * days,
         sales_conf_int_80_upper = conf_int_80_upper * days,
         sales_conf_int_90_lower = conf_int_90_lower * days,
         sales_conf_int_90_upper = conf_int_90_upper * days) |> 
  mutate(across(contains("conf_int"), ~ifelse(. < 0, 0, .))) |>
  mutate_if(is.numeric, round, 0) |> 
  as_tibble() |> 
  mutate(store_id = as.character(store_id),
         state_id = as.character(state_id),
         dept_id = as.character(dept_id),
         cat_id = as.character(cat_id)) |>
  select(store_id, 
         state_id, 
         dept_id, 
         cat_id, 
         key_date, 
         sales_amount,
         sales_conf_int_50_lower,
         sales_conf_int_50_upper,
         sales_conf_int_60_lower,
         sales_conf_int_60_upper,
         sales_conf_int_70_lower,
         sales_conf_int_70_upper,
         sales_conf_int_80_lower,
         sales_conf_int_80_upper,
         sales_conf_int_90_lower,
         sales_conf_int_90_upper
  )

fc_qty_best <- fc_qty_all |> 
  filter(!is_aggregated(dept_id), !is_aggregated(store_id), !is_aggregated(state_id), !is_aggregated(cat_id)) |> 
  left_join(best_models_qty, join_by(dept_id == dept_id,
                                     store_id == store_id,
                                       .model == .model), keep = FALSE) |>
  filter(keep == TRUE) |> 
  mutate(key_date = as.Date(month)) |> 
  mutate(conf_int_50 = hilo(daily_qty, 50),
         conf_int_60 = hilo(daily_qty, 60),
         conf_int_70 = hilo(daily_qty, 70),
         conf_int_80 = hilo(daily_qty, 80),
         conf_int_90 = hilo(daily_qty, 90)) |> 
  unpack_hilo("conf_int_50") |> 
  unpack_hilo("conf_int_60") |> 
  unpack_hilo("conf_int_70") |> 
  unpack_hilo("conf_int_80") |> 
  unpack_hilo("conf_int_90") |> 
  left_join(working_days, join_by(key_date == month), keep = FALSE) |> 
  mutate(qty_amount = .mean * days,
         qty_conf_int_50_lower = conf_int_50_lower * days,
         qty_conf_int_50_upper = conf_int_50_upper * days,
         qty_conf_int_60_lower = conf_int_60_lower * days,
         qty_conf_int_60_upper = conf_int_60_upper * days,
         qty_conf_int_70_lower = conf_int_70_lower * days,
         qty_conf_int_70_upper = conf_int_70_upper * days,
         qty_conf_int_80_lower = conf_int_80_lower * days,
         qty_conf_int_80_upper = conf_int_80_upper * days,
         qty_conf_int_90_lower = conf_int_90_lower * days,
         qty_conf_int_90_upper = conf_int_90_upper * days) |> 
  mutate(across(contains("conf_int"), ~ifelse(. < 0, 0, .))) |>
  mutate_if(is.numeric, round, 0) |> 
  as_tibble() |> 
  mutate(store_id = as.character(store_id),
         state_id = as.character(state_id),
         dept_id = as.character(dept_id),
         cat_id = as.character(cat_id)) |>
  select(store_id, 
         state_id, 
         dept_id, 
         cat_id, 
         key_date, 
         qty_amount,
         qty_conf_int_50_lower,
         qty_conf_int_50_upper,
         qty_conf_int_60_lower,
         qty_conf_int_60_upper,
         qty_conf_int_70_lower,
         qty_conf_int_70_upper,
         qty_conf_int_80_lower,
         qty_conf_int_80_upper,
         qty_conf_int_90_lower,
         qty_conf_int_90_upper
  )

final_fc <- fc_sales_best |> 
  left_join(fc_qty_best , join_by(store_id == store_id, state_id == state_id, dept_id == dept_id, cat_id == cat_id, key_date == key_date), keep = FALSE)

write_csv(final_fc, "data/fc.csv")
