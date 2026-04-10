# Data processing: Small boat activity in the English Channel

April 10, 2026

by [Isaac Arroyo](https://github.com/isaacarroyov), Data Visualisation Journalist

``` r
here::i_am("processing/processing.R")

library(tidyverse)

path2repo <- here::here()
path2input_data <- here::here("input_data")
path2output_data <- here::here("output_data")
```

## Intro

Data on migrants arriving in the UK in small boats is published weekly, every Friday, by the UK’s [Home Office](https://www.gov.uk/government/organisations/home-office) and [Border Force](https://www.gov.uk/government/organisations/border-force).

[Click here to visit the website and download the official data on Migrants detected crossing the English Channel in small boats](https://www.gov.uk/government/publications/migrants-detected-crossing-the-english-channel-in-small-boats).

This repository serves as an open access to the data, including additional statistics such as cumulative figures and the ratio of migrants per boat.

For any questions about the data, please contact migrationstatsenquiries@homeoffice.gov.uk; and for any questions on the processing or code, raise an issue or contact datavis@thenationalnews.com

> \[!NOTE\]
>
> The data published here is provisional and subject to change, including reduction. Finalised data on small boat crossings since 2018 is published in the quarterly Immigration system statistics under the topic “How many people come to the UK irregularly”.

The script creates 7 CSVs:

- Long format:
  - Daily arrivals of small boats and migrants
  - Weeekly arrivals of small boats and migrants
  - Monthly arrivals of small boats and migrants
  - Yearly arrivals of small boats and migrants
- Wide format:
  - Weeekly cumulative arrivals of migrants per year
  - Monthly cumulative arrivals of migrants per year

``` r
file_ts_migrants_boats <- list.files(
    path = path2input_data,
    pattern = "*.ods",
    full.names = TRUE)

data_source <- readODS::read_ods(
    path = file_ts_migrants_boats,
    sheet = 3) %>%
  janitor::clean_names() %>%
  mutate(date = dmy(date)) %>%
  rename(full_date = date) %>%
  arrange(full_date)
```

| full_date | migrants_arrived | boats_arrived | boats_arrived_involved_in_uncontrolled_landings | notes |
|:---|---:|---:|:---|:---|
| 2026-03-31 | 0 | 0 | 0 | NA |
| 2026-04-01 | 325 | 5 | 0 | NA |
| 2026-04-02 | 0 | 0 | 0 | NA |
| 2026-04-03 | 0 | 0 | 0 | NA |
| 2026-04-04 | 0 | 0 | 0 | NA |
| 2026-04-05 | 0 | 0 | 0 | NA |
| 2026-04-06 | 0 | 0 | 0 | NA |
| 2026-04-07 | 137 | 2 | 0 | NA |
| 2026-04-08 | 159 | 2 | 0 | NA |
| 2026-04-09 | 74 | 1 | 0 | NA |

> \[!NOTE\]
>
> For the rest of the script, the columns to use are `full_date`, `migrants_arrived` and `boats_arrived`

## Long format

Besides the date components (`n_year`, `n_month`, `n_day`, `n_week`, and `date_label`), the datasets will have three more columns:

- Ratio of migrants per boat (`migrants_per_boat`): All
- Cumulative number of migrant arrivals (`cumulative_migrants_arrived`): Except `db_yearly_migrants_boats`
- Cumulative number of small boats arrivals (`cumulative_boats_arrived`): Except `db_yearly_migrants_boats`

### Daily migrant and boats arrivals

``` r
db_daily_migrants_boats <- data_source %>%
  select(full_date, ends_with("arrived")) %>%
  # Create cumulative columns per year
  group_by(n_year = year(full_date)) %>%
  mutate(
    across(
      .cols = ends_with("arrived"),
      .fns = list("cumulative" = \(x) cumsum(x)),
      .names = "{.fn}_{.col}")) %>%
  ungroup() %>%
  # Ratio of migrants per boats
  mutate(
    boats_arrived = if_else(
        condition = boats_arrived > 0,
        true = boats_arrived,
        false = NA_real_),
    migrants_arrived = if_else(
        condition = migrants_arrived > 0,
        true = migrants_arrived,
        false = NA_real_),
    migrants_per_boat = if_else(
      condition = !is.na(boats_arrived) & !is.na(migrants_arrived),
      true = migrants_arrived / boats_arrived,
      false = NA_real_),
    migrants_per_boat_round = round(migrants_per_boat, digits = 0),
    n_month = month(full_date),
    label_month = month(full_date, label = TRUE, abbr = FALSE),
    n_day = day(full_date),
    date_label = str_glue("{label_month} {n_day}, {n_year}")) %>%
  relocate(migrants_per_boat, .after = boats_arrived) %>%
  relocate(n_year, .after = full_date) %>%
  relocate(n_month, .after = n_year) %>%
  relocate(n_day, .after = n_month) %>%
  relocate(date_label, .after = full_date) %>%
  select(!c(label_month))
```

**Daily migrants and small boats arrivals** (last ten rows)

| full_date | date_label | n_year | n_month | n_day | migrants_arrived | boats_arrived | migrants_per_boat | cumulative_migrants_arrived | cumulative_boats_arrived | migrants_per_boat_round |
|:---|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 2026-03-31 | March 31, 2026 | 2026 | 3 | 31 | NA | NA | NA | 4441 | 70 | NA |
| 2026-04-01 | April 1, 2026 | 2026 | 4 | 1 | 325 | 5 | 65.0 | 4766 | 75 | 65 |
| 2026-04-02 | April 2, 2026 | 2026 | 4 | 2 | NA | NA | NA | 4766 | 75 | NA |
| 2026-04-03 | April 3, 2026 | 2026 | 4 | 3 | NA | NA | NA | 4766 | 75 | NA |
| 2026-04-04 | April 4, 2026 | 2026 | 4 | 4 | NA | NA | NA | 4766 | 75 | NA |
| 2026-04-05 | April 5, 2026 | 2026 | 4 | 5 | NA | NA | NA | 4766 | 75 | NA |
| 2026-04-06 | April 6, 2026 | 2026 | 4 | 6 | NA | NA | NA | 4766 | 75 | NA |
| 2026-04-07 | April 7, 2026 | 2026 | 4 | 7 | 137 | 2 | 68.5 | 4903 | 77 | 68 |
| 2026-04-08 | April 8, 2026 | 2026 | 4 | 8 | 159 | 2 | 79.5 | 5062 | 79 | 80 |
| 2026-04-09 | April 9, 2026 | 2026 | 4 | 9 | 74 | 1 | 74.0 | 5136 | 80 | 74 |

### Weekly migrant and boat arrivals

In the database provided by the UK government there is a sheet with the weekly record of migrant arrivals, however the cutoff is at the end of that week. The problem with recording or grouping the data in this way is that week 53 exists in a leap year, or when the beginning and end of a year share a week, whether the first or the last. This temporal offset also causes a discrepancy in the final numbers.

As a solution, the following is done:

- Label the first days of January as week 1 if they fall in week 52 or 53
- Label the last days of December as week 52 if they fall in week 1 or 53

In this way, the end-of-year figures remain consistent across all the datasets created in this script.

``` r
db_weekly_migrants_boats <- db_daily_migrants_boats %>%
  # Ignore cumulative numbers and ratio of migrants per boat
  select(
    !c(starts_with("cumulative"),
       starts_with("ratio"))) %>%
  mutate(
    n_week = week(full_date),
    # Change the number of the week based on conditions
    n_week = case_when(
      n_week %in% c(52, 53) & n_month == 1 ~ 1,
      n_week %in% c(1, 53) & n_month == 12 ~ 52,
      .default = n_week)) %>%
  # Group by week and add up all the numbers
  group_by(n_year, n_week) %>%
  summarise(
    across(
      .cols = ends_with("arrived"),
      .fns = list("sum" = \(x) sum(x, na.rm = TRUE)),
      .names = "{.col}")) %>%
  ungroup() %>%
  arrange(n_year, n_week) %>%
  # Create cumulative columns per year
  group_by(n_year) %>%
  mutate(
    across(
      .cols = ends_with("arrived"),
      .fns = list("cumulative" = \(x) cumsum(x)),
      .names = "{.fn}_{.col}")) %>%
  ungroup() %>%
  # Ratio of migrants per boat
  mutate(
    boats_arrived = if_else(
      condition = boats_arrived > 0,
      true = boats_arrived,
      false = NA_real_),
    migrants_arrived = if_else(
      condition = migrants_arrived > 0,
      true = migrants_arrived,
      false = NA_real_),
    migrants_per_boat = if_else(
      condition = !is.na(boats_arrived) & !is.na(migrants_arrived),
      true = migrants_arrived / boats_arrived,
      false = NA_real_),
    migrants_per_boat_round = round(migrants_per_boat, digits = 0))
```

| n_year | n_week | migrants_arrived | boats_arrived | cumulative_migrants_arrived | cumulative_boats_arrived | migrants_per_boat | migrants_per_boat_round |
|---:|---:|---:|---:|---:|---:|---:|---:|
| 2026 | 6 | 541 | 8 | 1530 | 24 | 67.62500 | 68 |
| 2026 | 7 | NA | NA | 1530 | 24 | NA | NA |
| 2026 | 8 | 679 | 11 | 2209 | 35 | 61.72727 | 62 |
| 2026 | 9 | 479 | 7 | 2688 | 42 | 68.42857 | 68 |
| 2026 | 10 | 721 | 11 | 3409 | 53 | 65.54545 | 66 |
| 2026 | 11 | 310 | 5 | 3719 | 58 | 62.00000 | 62 |
| 2026 | 12 | 722 | 12 | 4441 | 70 | 60.16667 | 60 |
| 2026 | 13 | 325 | 5 | 4766 | 75 | 65.00000 | 65 |
| 2026 | 14 | 296 | 4 | 5062 | 79 | 74.00000 | 74 |
| 2026 | 15 | 74 | 1 | 5136 | 80 | 74.00000 | 74 |

### Monthly migrant and boat arrivals

``` r
db_monthly_migrants_boats <- db_daily_migrants_boats %>%
  select(!c(starts_with("cumulative"), starts_with("ratio"))) %>%
  group_by(date_month = floor_date(x = full_date, unit = "month")) %>%
  summarise(
    across(
      .cols = ends_with("arrived"),
      .fns = list("sum" = \(x) sum(x, na.rm = TRUE)),
      .names = "{.col}")) %>%
  ungroup() %>%
  arrange(date_month) %>%
  group_by(n_year = year(date_month)) %>%
  mutate(
    across(
      .cols = ends_with("arrived"),
      .fns = list("cumulative" = \(x) cumsum(x)),
      .names = "{.fn}_{.col}")) %>%
  ungroup() %>%
  mutate(
    boats_arrived = if_else(
      condition = boats_arrived > 0,
      true = boats_arrived,
      false = NA_real_),
    migrants_arrived = if_else(
      condition = migrants_arrived > 0,
      true = migrants_arrived,
      false = NA_real_),
    migrants_per_boat = if_else(
      condition = !is.na(boats_arrived) & !is.na(migrants_arrived),
      true = migrants_arrived / boats_arrived,
      false = NA_real_),
    migrants_per_boat_round = round(migrants_per_boat, digits = 0),
    n_month = month(date_month),
    date_label = format(x = date_month, format = "%B %Y")) %>%
  relocate(migrants_per_boat, .after = boats_arrived) %>%
  relocate(n_year, .after = date_month) %>%
  relocate(n_month, .after = n_year) %>%
  relocate(date_label, .after = date_month)
```

| date_month | date_label | n_year | n_month | migrants_arrived | boats_arrived | migrants_per_boat | cumulative_migrants_arrived | cumulative_boats_arrived | migrants_per_boat_round |
|:---|:---|---:|---:|---:|---:|---:|---:|---:|---:|
| 2025-07-01 | July 2025 | 2025 | 7 | 5454 | 88 | 61.97727 | 25436 | 431 | 62 |
| 2025-08-01 | August 2025 | 2025 | 8 | 3567 | 56 | 63.69643 | 29003 | 487 | 64 |
| 2025-09-01 | September 2025 | 2025 | 9 | 5084 | 72 | 70.61111 | 34087 | 559 | 71 |
| 2025-10-01 | October 2025 | 2025 | 10 | 2867 | 44 | 65.15909 | 36954 | 603 | 65 |
| 2025-11-01 | November 2025 | 2025 | 11 | 2338 | 33 | 70.84848 | 39292 | 636 | 71 |
| 2025-12-01 | December 2025 | 2025 | 12 | 2180 | 36 | 60.55556 | 41472 | 672 | 61 |
| 2026-01-01 | January 2026 | 2026 | 1 | 933 | 15 | 62.20000 | 933 | 15 | 62 |
| 2026-02-01 | February 2026 | 2026 | 2 | 1276 | 20 | 63.80000 | 2209 | 35 | 64 |
| 2026-03-01 | March 2026 | 2026 | 3 | 2232 | 35 | 63.77143 | 4441 | 70 | 64 |
| 2026-04-01 | April 2026 | 2026 | 4 | 695 | 10 | 69.50000 | 5136 | 80 | 70 |

### Yearly migrant and boat arrivals

``` r
db_yearly_migrants_boats <- db_daily_migrants_boats %>%
  select(!c(starts_with("cumulative"), starts_with("ratio"))) %>%
  group_by(n_year) %>%
  summarise(
    across(
      .cols = ends_with("arrived"),
      .fns = list("sum" = \(x) sum(x, na.rm = TRUE)),
      .names = "{.col}")) %>%
  ungroup() %>%
  mutate(
    migrants_per_boat = migrants_arrived / boats_arrived,
    migrants_per_boat_round = round(migrants_per_boat, digits = 0))
```

| n_year | migrants_arrived | boats_arrived | migrants_per_boat | migrants_per_boat_round |
|---:|---:|---:|---:|---:|
| 2018 | 299 | 43 | 6.953488 | 7 |
| 2019 | 1843 | 164 | 11.237805 | 11 |
| 2020 | 8462 | 641 | 13.201248 | 13 |
| 2021 | 28526 | 1034 | 27.588008 | 28 |
| 2022 | 45755 | 1110 | 41.220721 | 41 |
| 2023 | 29437 | 602 | 48.898671 | 49 |
| 2024 | 36816 | 695 | 52.972662 | 53 |
| 2025 | 41472 | 672 | 61.714286 | 62 |
| 2026 | 5136 | 80 | 64.200000 | 64 |

## Wide format

**Weekly cumulative migrant arrivals**

``` r
wide_weekly_cumulative_migrants <- db_weekly_migrants_boats %>%
  select(n_year, n_week, cumulative_migrants_arrived) %>%
  pivot_wider(
    names_from = n_year,
    values_from = cumulative_migrants_arrived)
```

| n_week | 2018 | 2019 | 2020 | 2021 | 2022 | 2023 | 2024 | 2025 | 2026 |
|-------:|-----:|-----:|-----:|-----:|-----:|-----:|-----:|-----:|-----:|
|      6 |    7 |   45 |  277 |  286 | 1339 | 2124 | 1553 | 1554 | 1530 |
|      7 |    7 |   89 |  277 |  286 | 1350 | 2589 | 1716 | 1893 | 1530 |
|      8 |    7 |  111 |  277 |  419 | 1350 | 2953 | 2006 | 2056 | 2209 |
|      9 |    7 |  133 |  277 |  632 | 2212 | 2953 | 2582 | 3224 | 2688 |
|     10 |    7 |  133 |  305 |  797 | 2212 | 3201 | 3406 | 4395 | 3409 |
|     11 |    7 |  133 |  330 |  797 | 3229 | 3565 | 3468 | 4395 | 3719 |
|     12 |    7 |  157 |  369 | 1134 | 4162 | 3683 | 4306 | 6049 | 4441 |
|     13 |    7 |  199 |  465 | 1393 | 4548 | 3793 | 5435 | 6642 | 4766 |
|     14 |    7 |  207 |  658 | 1393 | 4548 | 4670 | 5435 | 7228 | 5062 |
|     15 |    7 |  207 |  821 | 1524 | 6009 | 4899 | 6265 | 8888 | 5136 |

**Monhtly cumulative migrant arrivals**

``` r
wide_monthly_cumulative_migrants <- db_monthly_migrants_boats %>%
  select(n_year, n_month, cumulative_migrants_arrived) %>%
  pivot_wider(
    names_from = n_year,
    values_from = cumulative_migrants_arrived) %>%
  mutate(
    month_short = month(n_month, label = TRUE, abbr = TRUE),
    month_long = month(n_month, label = TRUE, abbr = FALSE)
  ) %>%
  relocate(month_short, .after = n_month) %>%
  relocate(month_long, .after = month_short) %>%
  select(!n_month)
```

| month_short | month_long | 2018 | 2019 | 2020 | 2021 | 2022 | 2023 | 2024 |  2025 | 2026 |
|:------------|:-----------|-----:|-----:|-----:|-----:|-----:|-----:|-----:|------:|-----:|
| Jan         | January    |    7 |   45 |   94 |  224 | 1339 | 1180 | 1335 |  1098 |  933 |
| Feb         | February   |    7 |  133 |  277 |  532 | 1482 | 2953 | 2255 |  2056 | 2209 |
| Mar         | March      |    7 |  199 |  465 | 1363 | 4548 | 3793 | 5435 |  6642 | 4441 |
| Apr         | April      |    7 |  279 | 1023 | 2113 | 6691 | 5946 | 7567 | 11074 | 5136 |

## Save data

### … long format

``` r
write_csv(
  x = db_daily_migrants_boats,
  file = here::here(path2output_data, "long_uk_small_boats_daily.csv"),
  na = "")

write_csv(
  x = db_weekly_migrants_boats,
  file = here::here(path2output_data, "long_uk_small_boats_weekly.csv"),
  na = "")

write_csv(
  x = db_monthly_migrants_boats,
  file = here::here(path2output_data, "long_uk_small_boats_monthly.csv"),
  na = "")

write_csv(
  x = db_yearly_migrants_boats,
  file = here::here(path2output_data, "long_uk_small_boats_yearly.csv"),
  na = "")
```

### … wide format

``` r
write_csv(
  x = wide_weekly_cumulative_migrants,
  file = here::here(path2output_data, "wide_uk_small_boats_weekly_cumulative_migrants.csv"),
  na = "")

write_csv(
  x = wide_monthly_cumulative_migrants,
  file = here::here(path2output_data, "wide_uk_small_boats_monthly_cumulative_migrants.csv"),
  na = "")
```
