# Load required package, install if not present
if (!require("pacman")) {
  install.packages("pacman")
}

# Use 'pacman' package to load necessary packages for data manipulation, time series analysis, and visualization.
pacman::p_load(
  anomalize,   # For anomaly detection
  dplyr,       # For data manipulation
  ggplot2,      # For plotting
  tsibble,     # For time series data manipulation
  lubridate,   # For date-time manipulation
  tidyr,       # For data tidying
  timetk,      # For time series anomaly cleaning
  timeDate     # For handling date-time data
)

# Defining a negation function for set operations
`%!in%` <- Negate(`%in%`)

# Reading and summarizing cleaned data from an RDS file
cleaned_data <- readRDS("data/cleaned_data.RDS") |> 
  summarize(.by = c(dept_id, cat_id, store_id, state_id, month),
            qty_sold = sum(qty_sold),
            sales_amount = sum(sales_amount)) |> 
  mutate(month_end = ceiling_date(month, "month") - days(1))

# Determining the range of dates in the cleaned data
date_max <- as.Date(paste0(year(max(cleaned_data$month_end)), "-12-31"))
date_min <- min(cleaned_data$month)

# Setting system locale to English for consistent date handling
Sys.setlocale("LC_TIME", "English")

# Generating a sequence of dates between the minimum and maximum date
all_dates <- data.frame(Date = seq(from = date_min, to = date_max, by = "day"))

# Filtering data to exclude weekends
weekdays_only <- all_dates  |>  
  filter(!weekdays(Date) %in% c("Saturday", "Sunday"))

# Defining US federal holidays within a specified range
holidays <- as.Date(c(
  # New Year's Day
  "2011-01-01", "2012-01-01", "2013-01-01", "2014-01-01", "2015-01-01", "2016-01-01",
  # Martin Luther King Jr. Day (third Monday of January)
  "2011-01-17", "2012-01-16", "2013-01-21", "2014-01-20", "2015-01-19", "2016-01-18",
  # Washington's Birthday (third Monday of February)
  "2011-02-21", "2012-02-20", "2013-02-18", "2014-02-17", "2015-02-16", "2016-02-15",
  # Memorial Day (last Monday of May)
  "2011-05-30", "2012-05-28", "2013-05-27", "2014-05-26", "2015-05-25", "2016-05-30",
  # Independence Day
  "2011-07-04", "2012-07-04", "2013-07-04", "2014-07-04", "2015-07-04", "2016-07-04",
  # Labor Day (first Monday of September)
  "2011-09-05", "2012-09-03", "2013-09-02", "2014-09-01", "2015-09-07", "2016-09-05",
  # Columbus Day (second Monday of October)
  "2011-10-10", "2012-10-08", "2013-10-14", "2014-10-13", "2015-10-12", "2016-10-10",
  # Veterans Day
  "2011-11-11", "2012-11-11", "2013-11-11", "2014-11-11", "2015-11-11", "2016-11-11",
  # Thanksgiving Day (fourth Thursday of November)
  "2011-11-24", "2012-11-22", "2013-11-28", "2014-11-27", "2015-11-26", "2016-11-24",
  # Christmas Day
  "2011-12-25", "2012-12-25", "2013-12-25", "2014-12-25", "2015-12-25", "2016-12-25"
  ))

# Removing holiday dates from the weekdays dataset
working_days <- weekdays_only |> filter(Date %!in% holidays) |> 
  mutate(month = floor_date(Date, "month")) |> 
  summarize(.by = month,
            days = n())

# Saving the working_days dataset
saveRDS(working_days, "data/working_days.RDS")

cleaned_data |> 
  filter(dept_id == "HOBBIES_1",
         store_id == "CA_1") |> 
  time_decompose(target = sales_amount, method = "stl")  |>  
  select(1:5) |> 
  pivot_longer(-month, names_to = "component", values_to = "value") |> 
  mutate(component = factor(component, levels = c("observed", "trend", "season", "remainder"))) |> 
  ggplot(aes(x = month, y = value)) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        text = element_text(size = 14)) +
  scale_y_continuous(labels = scales::comma) +
  geom_line(color = "steelblue") +
  theme_minimal() +
  labs(x = NULL,
       y = NULL) +
  facet_wrap(vars(component),
             ncol = 1,
             scales = "free")

cleaned_data |> 
  filter(dept_id == "HOBBIES_1",
         store_id == "CA_1") |> 
  timetk::anomalize(.date_var = month,
                    .value = sales_amount,
                    .iqr_alpha = 0.1) |>
  timetk::plot_anomalies(.date_var = month, .interactive = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  labs(title = NULL) +
  theme(legend.position = "none")


cleaned_data |> 
  filter(dept_id == "HOBBIES_1",
         store_id == "CA_1") |> 
  timetk::anomalize(.date_var = month,
                    .value = sales_amount,
                    .iqr_alpha = 0.1) |>
  mutate(cleaned = round(observed_clean, 0)) |> 
  select(month, observed, cleaned) |> 
  pivot_longer(-month, names_to = "series", values_to = "sales_amount") |> 
  ggplot(aes(x = month, y = sales_amount, color = series)) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(x = NULL,
       y = NULL) + 
  theme(legend.position = "top",
        legend.title = element_blank())


# Detecting and removing anomalies in sales data
cleaned_data_sales <- cleaned_data |> 
  as_tibble() |> 
  group_by(state_id, store_id, cat_id, dept_id) |> 
  anomalize(
    .date_var      = month, 
    .value         = sales_amount,
    .iqr_alpha     = 0.1,
    .message       = FALSE
  ) |> 
  select(state_id, store_id, cat_id, dept_id, month, sales_amount = observed_clean) |> 
  mutate(sales_amount = round(sales_amount, 0)) |> 
  ungroup()

# Detecting and removing anomalies in quantity sold data
cleaned_data_final <- cleaned_data |> 
  as_tibble() |> 
  group_by(state_id, store_id, cat_id, dept_id) |> 
  anomalize(
    .date_var      = month, 
    .value         = qty_sold,
    .iqr_alpha     = 0.1,
    .message       = FALSE
  ) |> 
  select(state_id, store_id, cat_id, dept_id, month, qty_sold = observed_clean) |> 
  mutate(qty_sold = round(qty_sold, 0)) |> 
  ungroup() |> 
  left_join(cleaned_data_sales, by = c("state_id", "store_id", "cat_id", "dept_id", "month")) |> 
  left_join(working_days, by = "month") |> 
  mutate(daily_sales = round(sales_amount/days,0),
         daily_qty = round(qty_sold/days,0)) |> 
  select(-days) |> 
  mutate(month = yearmonth(month)) |> 
  as_tsibble(key = c(state_id, store_id, cat_id, dept_id),
             index = month,
             regular = TRUE) |>
  fill_gaps(daily_sales = 0, daily_qty = 0)

# Saving the final cleaned dataset
saveRDS(cleaned_data_final, "data/cleaned_data_final.RDS")
