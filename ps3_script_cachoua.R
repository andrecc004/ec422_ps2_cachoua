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
