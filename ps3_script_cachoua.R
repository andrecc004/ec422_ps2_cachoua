# EC 422 – Problem Set 3
# Andre Cachoua
# All code for Parts 2–5, in the order it was run.
# Data: data/retail_sales_raw.csv (FRED RSAFS + RSAFSNA, downloaded manually)

# ===================== Part 2: Time-Series Data in R =====================

# ---- 2.2 Read the .csv and inspect the date column ----
library(tidyverse)

retail_raw <- read_csv("data/retail_sales_raw.csv")

head(retail_raw)   # 2.2a: observation_date is read as <date>

# 2.2b: re-read with explicit column types
retail_raw <- read_csv(
  "data/retail_sales_raw.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    RSAFS = col_double(),
    RSAFSNA = col_double()
  )
)

head(retail_raw)

# ---- 2.3 Reshape to long format ----
retail_raw_long <- retail_raw |>
  select(observation_date, RSAFS, RSAFSNA) |>
  pivot_longer(cols = c(RSAFS, RSAFSNA), names_to = "series_id", values_to = "sales")

head(retail_raw_long)

# ---- 2.4 Build the tsibble ----
library(fpp3)

retail_ts <- retail_raw_long |>
  mutate(Month = yearmonth(observation_date)) |>
  as_tsibble(index = Month, key = series_id)

head(retail_ts)
has_gaps(retail_ts)

# ---- 2.5 Canadian gas production ----
head(canadian_gas)

key_vars(canadian_gas)
index_var(canadian_gas)

can_gas <- canadian_gas |>
  select(Month, Volume)

has_gaps(can_gas)

head(can_gas)

# ---- 2.6 Working with dates in a tsibble ----
# 2.6a Filtering by date
can_gas_recent <- can_gas |>
  filter(Month >= yearmonth("1990 Jan"))

head(can_gas_recent)

# 2.6b Extracting parts of a date
can_gas_recent <- can_gas_recent |>
  mutate(year    = year(Month),
         month   = month(Month, label = TRUE),
         quarter = quarter(Month))

can_gas_recent |>
  as_tibble() |>
  group_by(month) |>
  summarize(avg_volume = mean(Volume))

# 2.6c Date arithmetic
can_gas_recent |>
  mutate(next_month           = Month + 1,
         same_month_last_year = Month - 12) |>
  head()

retail_raw |>
  mutate(next_day = observation_date + 1) |>
  select(observation_date, next_day) |>
  head()

# 2.6d Changing the frequency (monthly -> quarterly)
can_gas_q <- can_gas |>
  filter(Month <= yearmonth("2004 Dec")) |>
  index_by(Quarter = yearquarter(Month)) |>
  summarize(Volume = sum(Volume))

head(can_gas_q)

can_gas_q |> autoplot(Volume)

# ---- 2.7 Plotting with autoplot() ----
# 2.7a
retail_ts |> autoplot(sales)

# 2.7b
retail_ts |>
  autoplot(sales) +
  labs(
    title = "U.S. Retail and Food Services Sales",
    subtitle = "Seasonally adjusted vs. not seasonally adjusted",
    x = NULL, y = "Millions of dollars", color = NULL
  ) +
  theme_minimal()

# 2.7c
can_gas |>
  autoplot(Volume) +
  labs(
    title = "Canadian Natural Gas Production",
    subtitle = "Monthly, January 1960 – February 2005",
    x = NULL, y = "Billions of cubic meters"
  ) +
  theme_minimal()

# 2.7d: add a LOESS smoother and comma-formatted y-axis to the retail plot
retail_ts |>
  autoplot(sales) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 0.8, linetype = "dashed") +
  scale_y_continuous(labels = scales::label_dollar(scale = 1e-3, suffix = "B")) +
  labs(
    title = "U.S. Retail and Food Services Sales with LOESS Trend",
    x = NULL, y = "Sales (billions of dollars)", color = NULL
  ) +
  theme_minimal()

# ===================== Part 3: Transformations =====================

# ---- 3.1 Log transformation ----
# 3.1b
can_gas <- can_gas |>
  mutate(log_volume = log(Volume))

can_gas |>
  autoplot(log_volume) +
  labs(title = "Log of Canadian Gas Production", x = NULL, y = "log(Volume)") +
  theme_minimal()

# 3.1c
retail_nsa <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  mutate(log_sales = log(sales))

retail_nsa |>
  autoplot(sales) +
  labs(title = "NSA Retail Sales (Level)", x = NULL, y = "Millions of dollars") +
  theme_minimal()

retail_nsa |>
  autoplot(log_sales) +
  labs(title = "NSA Retail Sales (Log)", x = NULL, y = "log(sales)") +
  theme_minimal()

# ---- 3.2 Growth rates ----
can_gas <- can_gas |>
  mutate(
    growth_direct = 100 * difference(Volume) / lag(Volume),
    growth_logdiff = 100 * difference(log_volume)
  )

can_gas |>
  autoplot(growth_direct) +
  labs(title = "Canadian Gas Production: Month-over-Month Growth",
       x = NULL, y = "Growth rate (%)") +
  theme_minimal()

can_gas <- can_gas |>
  mutate(growth_gap = abs(growth_direct - growth_logdiff))

can_gas |>
  autoplot(growth_gap) +
  labs(title = "Gap Between Exact and Log-Difference Growth Rates",
       x = NULL, y = "Absolute difference (percentage points)") +
  theme_minimal()

summary(can_gas$growth_gap)

# ---- 3.3 SA vs. NSA growth rates ----
retail_ts <- retail_ts |>
  group_by_key() |>
  mutate(growth_mom = 100 * difference(log(sales))) |>
  ungroup()

retail_ts |>
  autoplot(growth_mom) +
  labs(
    title = "Month-over-Month Growth: SA vs. NSA Retail Sales",
    x = NULL, y = "Growth rate (%)", color = NULL
  ) +
  theme_minimal()

retail_ts |>
  as_tibble() |>
  group_by(series_id) |>
  summarize(sd_growth = sd(growth_mom, na.rm = TRUE))

# ===================== Part 4: Seasonality =====================

# ---- 4.1 Season plots ----
# 4.1a
retail_ts |>
  filter(series_id == "RSAFSNA") |>
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in NSA Retail Sales",
    y = "Millions of dollars"
  ) +
  theme_minimal()

# 4.1b
retail_ts |>
  filter(series_id == "RSAFS") |>
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in SA Retail Sales",
    y = "Millions of dollars"
  ) +
  theme_minimal()

# 4.1c
can_gas |>
  gg_season(log_volume) +
  labs(
    title = "Seasonal Pattern in Canadian Gas Production",
    y = "log(Volume)"
  ) +
  theme_minimal()

# ---- 4.2 Subseries plot ----
can_gas |>
  gg_subseries(log_volume) +
  labs(
    title = "Subseries Plot: Canadian Gas Production",
    y = "log(Volume)"
  )

# ---- 4.3 X-13ARIMA-SEATS ----
# (install.packages("seasonal") once from the Console if needed)
# 4.3a
retail_nsa_x13 <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  model(x13 = X_13ARIMA_SEATS(sales)) |>
  components()

retail_nsa_x13 |> autoplot()

# average seasonal factor by calendar month
retail_nsa_x13 |>
  as_tibble() |>
  mutate(month = month(Month, label = TRUE)) |>
  group_by(month) |>
  summarize(avg_seasonal = mean(seasonal),
            min_seasonal = min(seasonal),
            max_seasonal = max(seasonal))

# 4.3b
sa_compare <- retail_nsa_x13 |>
  as_tibble() |>
  select(Month, mine = season_adjust) |>
  left_join(
    retail_ts |>
      filter(series_id == "RSAFS") |>
      as_tibble() |>
      transmute(Month, census = sales),
    by = "Month"
  ) |>
  mutate(pct_diff = 100 * (log(mine) - log(census)))

sa_compare |>
  ggplot(aes(x = Month, y = pct_diff)) +
  geom_line() +
  labs(
    title = "My X-13 Seasonal Adjustment Minus the Census Bureau's",
    x = NULL, y = "Difference (%)"
  ) +
  theme_minimal()

summary(sa_compare$pct_diff)
sa_compare |> slice_max(abs(pct_diff), n = 5)

# ---- 4.4 Linear trend + seasonal dummies ----
# 4.4a
can_gas_fit <- can_gas |>
  model(TSLM(log_volume ~ trend() + season()))

report(can_gas_fit)

can_gas_fit |>
  tidy() |>
  select(term, estimate, std.error, statistic, p.value) |>
  knitr::kable(digits = 3)

# 4.4b
can_gas_fit |>
  augment() |>
  ggplot(aes(x = Month)) +
  geom_line(aes(y = log_volume, color = "Actual")) +
  geom_line(aes(y = .fitted, color = "Fitted")) +
  labs(title = "Linear Trend + Seasonal Dummies Fit: Canadian Gas Production",
       y = "log(Volume)", color = NULL) +
  theme_minimal()

# 4.4c
can_gas_fit |> gg_tsresiduals()

can_gas_fit |>
  augment() |>
  features(.innov, ljung_box, lag = 24)

# ===================== Part 5: Forecast Evaluation =====================

# ---- 5.1 Expanding-window TS-CV ----
gas_cv_fc <- can_gas |>
  select(Month, Volume) |>
  stretch_tsibble(.init = 240, .step = 1) |>
  model(snaive = SNAIVE(Volume),
        tslm   = TSLM(log(Volume) ~ trend() + season())) |>
  forecast(h = 1)

gas_cv_fc |>
  accuracy(can_gas, measures = list(MAE = MAE, RMSE = RMSE))

# first and last evaluated forecast dates
gas_cv_fc |>
  as_tibble() |>
  semi_join(as_tibble(can_gas), by = "Month") |>
  summarize(first = min(Month), last = max(Month), n = n_distinct(Month))

# ---- 5.2 Diebold–Mariano test ----
errs <- gas_cv_fc |>
  as_tibble() |>
  left_join(as_tibble(can_gas) |> select(Month, actual = Volume),
            by = "Month") |>
  mutate(e = actual - .mean) |>
  select(Month, .model, e) |>
  pivot_wider(names_from = .model, values_from = e) |>
  na.omit()

forecast::dm.test(errs$snaive, errs$tslm, h = 1, power = 1)

# ---- 5.3 Rolling-window TS-CV ----
gas_cv_fc_roll <- can_gas |>
  select(Month, Volume) |>
  slide_tsibble(.size = 240, .step = 1) |>
  model(snaive = SNAIVE(Volume),
        tslm   = TSLM(log(Volume) ~ trend() + season())) |>
  forecast(h = 1)

gas_cv_fc_roll |>
  accuracy(can_gas, measures = list(MAE = MAE, RMSE = RMSE))
