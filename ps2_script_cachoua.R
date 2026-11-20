library(tidyverse)
library(fpp3)
library(BVAR)


# ---- 2.1a ----
liquor_raw <- read_csv(
  "data/MRTSSM4453USN.csv",na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    MRTSSM4453USN = col_double()
  )
)

liquor <- liquor_raw |>
  transmute(Month = yearmonth(observation_date), sales = MRTSSM4453USN) |>
  as_tsibble(index = Month) |>
  filter(Month <= yearmonth("2019 Dec"))

has_gaps(liquor)

# ---- 2.1b ----
liquor |>
  autoplot(sales) +
  labs(title = "U.S Beer, Wine, and Liquor Store Sales",
       x = "Month", y = "Millions of dollars")

liquor |>
  gg_season(log(sales)) +
  labs(title = "Seasonal Plot Log Liquor Store Sales",
       x = "Month", y = "log sales")

#  2.1b:
# Sales trend steadily upward. December is the highest month by far;
# January and February are the lowest. The seasonal swings grow as the
# level rises, so taking logs makes them roughly constant in size.

# ---- 2.2a ----