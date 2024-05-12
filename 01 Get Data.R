# Check and install 'pacman' package if not already installed
if (!require("pacman")) {
  install.packages("pacman")
}

# Load 'tidyverse' package using 'pacman' for data manipulation
pacman::p_load(
  tidyverse       # Comprehensive collection of data manipulation tools
)

# Load sales data, calendar information, and prices from CSV files
sales_data <- read.csv("data/sales_train_evaluation.csv")
calendar <- read.csv("data/calendar.csv") |> 
  select(d, date, wm_yr_wk)  # Select relevant columns from calendar
prices <- read.csv("data/sell_prices.csv")

# Transform and clean sales data
cleaned_data <- sales_data |> 
  pivot_longer(-c(1:6),  # Convert wide format to long format, excluding first six columns
               names_to = "date_id",
               values_to = "qty_sold") |> 
  left_join(calendar,  # Merge with calendar data
            by = c("date_id" = "d")) |> 
  select(-date_id) |>  # Drop 'date_id' after merge
  mutate(date = as.Date(date)) |>  # Convert 'date' column to Date type
  left_join(prices,  # Merge with price data
            by = c("store_id", "item_id", "wm_yr_wk")) |> 
  select(-wm_yr_wk) |>  # Drop 'wm_yr_wk' column after merge
  mutate(month = floor_date(date, unit = "month"),  # Extract month from 'date'
         sales_amount = qty_sold * sell_price) |>  # Calculate sales amount
  summarize(.by = c(item_id, dept_id, cat_id, store_id, state_id, month),
            qty_sold = sum(qty_sold, na.rm = TRUE),  # Sum quantities sold, handling NA values
            sales_amount = sum(sales_amount, na.rm = TRUE))  # Sum sales amounts, handling NA values

# Save cleaned and summarized data to an RDS file
saveRDS(cleaned_data, "data/cleaned_data.RDS")
