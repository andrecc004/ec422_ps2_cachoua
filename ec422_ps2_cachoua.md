---
title: "ec422_ps2_cachoua"
author: "Andre Cachoua"
date: "2026-10-16"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

'## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r cars}
summary(cars)
```

## Including Plots

You can also embed plots, for example:

```{r pressure, echo=FALSE}
plot(pressure)
```


```{r}
unemp <- unemp_raw |>
  rename(unemployment_rate = price) |>
  mutate(
    region = case_when(
      symbol == "UNRATE" ~ "United States",
      symbol == "ORUR"    ~ "Oregon",
      symbol == "NYUR"    ~ "New York",
      symbol == "CAUR"    ~ "California",
      symbol == "TXUR"    ~ "Texas",
    )
  )
```


```{r}
unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(
    unemployment_rate = replace(
      unemployment_rate,
      date == as.Date("2025-10-01"),
      unemployment_rate[date == as.Date("2025-09-01")]
    )
  ) |>
  ungroup()
```

```{r}
# Observations from January 2015 onward
unemp_recent <- unemp |>
  filter(date >= as.Date("2015-01-01"))

# Full-sample summary by region
unemp_summary <- unemp |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate  = min(unemployment_rate, na.rm = TRUE),
    max_rate  = max(unemployment_rate, na.rm = TRUE)
  )

# Same summary, 2015 onward
unemp_recent_summary <- unemp_recent |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate  = min(unemployment_rate, na.rm = TRUE),
    max_rate  = max(unemployment_rate, na.rm = TRUE)
  )
```

```{r}
unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(unemployment_rate_change = unemployment_rate - lag(unemployment_rate)) |>
  ungroup()
```

```{r}
unemp_top_increases <- unemp |>
  slice_max(unemployment_rate_change, n = 5, with_ties = TRUE)

unemp_top_decreases <- unemp |>
  slice_min(unemployment_rate_change, n = 5, with_ties = TRUE)
```

```{r}
unemp_no_covid <- unemp |>
  filter(year(date) != 2020)

unemp_top_increases_no_covid <- unemp_no_covid |>
  slice_max(unemployment_rate_change, n = 5, with_ties = TRUE)

unemp_top_decreases_no_covid <- unemp_no_covid |>
  slice_min(unemployment_rate_change, n = 5, with_ties = TRUE)
```

```{r}
ggplot(unemp, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line() +
  labs(
    title = "Monthly Unemployment Rate U.S. and States",
    x = "Date", y = "Unemployment rate (%)", color = "Region"
  ) +
  theme_minimal()
```

```{r}
ggplot(unemp, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line() +
  facet_wrap(~ region) +
  labs(
    title = "Monthly Unemployment Rate by Region",
    x = "Date", y = "Unemployment rate %"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
```

```{r}
ggplot(unemp_summary, aes(x = fct_reorder(region, mean_rate), y = mean_rate)) +
  geom_col() +
  labs(
    title = "Average Unemployment Rate by Region, 1976 to Present",
    x = "Region", y = "Mean unemployment rate %"
  ) +
  theme_minimal()
```

```{r}
unemp |>
  filter(region %in% c("Oregon", "United States")) |>
  select(date, region, unemployment_rate) |>
  pivot_wider(names_from = region, values_from = unemployment_rate) |>
  mutate(gap = Oregon - `United States`) |>
  ggplot(aes(x = date, y = gap)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Oregon Unemployment Rate Minus U.S. Rate",
    x = "Date", y = "Percentage points"
  ) +
  theme_minimal()
```

```{r}
```

```{r}
```

```{r}
```