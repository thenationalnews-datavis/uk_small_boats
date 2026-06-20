# Data processing: Small boat activity in the English Channel

June 20, 2026

by [Isaac Arroyo](https://github.com/isaacarroyov), Data Visualisation Journalist

``` r
here::i_am("processing/processing.R")

library(tidyverse)

path2repo <- here::here()
path2input_data <- here::here("input_data")
path2output_data <- here::here("output_data")

date_last_update <- as.Date("2026-06-19")
text_file_last_update <- format(x = date_last_update, format = "%e_%B_%Y") %>% str_squish()

# Download and save data
url_download <- str_glue("https://assets.publishing.service.gov.uk/media/6a106e9c0a1a96d9418d281c/{text_file_last_update}_Small_boats_-_time_series.ods")
curl::curl_download(
    url = url_download,
    destfile = here::here(path2input_data, "data_small_boats_time_series.ods"))
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
| 2026-06-09 | 0 | 0 | 0 | NA |
| 2026-06-10 | 0 | 0 | 0 | NA |
| 2026-06-11 | 0 | 0 | 0 | NA |
| 2026-06-12 | 0 | 0 | 0 | NA |
| 2026-06-13 | 0 | 0 | 0 | NA |
| 2026-06-14 | 0 | 0 | 0 | NA |
| 2026-06-15 | 710 | 11 | 0 | NA |
| 2026-06-16 | 0 | 0 | 0 | NA |
| 2026-06-17 | 0 | 0 | 0 | NA |
| 2026-06-18 | 392 | 6 | 0 | NA |

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
| 2026-06-09 | June 9, 2026 | 2026 | 6 | 9 | NA | NA | NA | 9142 | 142 | NA |
| 2026-06-10 | June 10, 2026 | 2026 | 6 | 10 | NA | NA | NA | 9142 | 142 | NA |
| 2026-06-11 | June 11, 2026 | 2026 | 6 | 11 | NA | NA | NA | 9142 | 142 | NA |
| 2026-06-12 | June 12, 2026 | 2026 | 6 | 12 | NA | NA | NA | 9142 | 142 | NA |
| 2026-06-13 | June 13, 2026 | 2026 | 6 | 13 | NA | NA | NA | 9142 | 142 | NA |
| 2026-06-14 | June 14, 2026 | 2026 | 6 | 14 | NA | NA | NA | 9142 | 142 | NA |
| 2026-06-15 | June 15, 2026 | 2026 | 6 | 15 | 710 | 11 | 64.54545 | 9852 | 153 | 65 |
| 2026-06-16 | June 16, 2026 | 2026 | 6 | 16 | NA | NA | NA | 9852 | 153 | NA |
| 2026-06-17 | June 17, 2026 | 2026 | 6 | 17 | NA | NA | NA | 9852 | 153 | NA |
| 2026-06-18 | June 18, 2026 | 2026 | 6 | 18 | 392 | 6 | 65.33333 | 10244 | 159 | 65 |

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
| 2026 | 16 | 665 | 10 | 6077 | 94 | 66.50000 | 66 |
| 2026 | 17 | 339 | 6 | 6416 | 100 | 56.50000 | 56 |
| 2026 | 18 | 894 | 15 | 7310 | 115 | 59.60000 | 60 |
| 2026 | 19 | 266 | 4 | 7576 | 119 | 66.50000 | 66 |
| 2026 | 20 | NA | NA | 7576 | 119 | NA | NA |
| 2026 | 21 | 1202 | 17 | 8778 | 136 | 70.70588 | 71 |
| 2026 | 22 | 364 | 6 | 9142 | 142 | 60.66667 | 61 |
| 2026 | 23 | NA | NA | 9142 | 142 | NA | NA |
| 2026 | 24 | 710 | 11 | 9852 | 153 | 64.54545 | 65 |
| 2026 | 25 | 392 | 6 | 10244 | 159 | 65.33333 | 65 |

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
| 2025-09-01 | September 2025 | 2025 | 9 | 5084 | 72 | 70.61111 | 34087 | 559 | 71 |
| 2025-10-01 | October 2025 | 2025 | 10 | 2867 | 44 | 65.15909 | 36954 | 603 | 65 |
| 2025-11-01 | November 2025 | 2025 | 11 | 2338 | 33 | 70.84848 | 39292 | 636 | 71 |
| 2025-12-01 | December 2025 | 2025 | 12 | 2180 | 36 | 60.55556 | 41472 | 672 | 61 |
| 2026-01-01 | January 2026 | 2026 | 1 | 933 | 15 | 62.20000 | 933 | 15 | 62 |
| 2026-02-01 | February 2026 | 2026 | 2 | 1276 | 20 | 63.80000 | 2209 | 35 | 64 |
| 2026-03-01 | March 2026 | 2026 | 3 | 2232 | 35 | 63.77143 | 4441 | 70 | 64 |
| 2026-04-01 | April 2026 | 2026 | 4 | 1975 | 30 | 65.83333 | 6416 | 100 | 66 |
| 2026-05-01 | May 2026 | 2026 | 5 | 2726 | 42 | 64.90476 | 9142 | 142 | 65 |
| 2026-06-01 | June 2026 | 2026 | 6 | 1102 | 17 | 64.82353 | 10244 | 159 | 65 |

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
| 2026 | 10244 | 159 | 64.427673 | 64 |

## Wide format

**Weekly cumulative migrant arrivals**

``` r
wide_weekly_cumulative_migrants <- db_weekly_migrants_boats %>%
  select(n_year, n_week, cumulative_migrants_arrived) %>%
  pivot_wider(
    names_from = n_year,
    values_from = cumulative_migrants_arrived)
```

| n_week | 2018 | 2019 | 2020 | 2021 |  2022 |  2023 |  2024 |  2025 |  2026 |
|-------:|-----:|-----:|-----:|-----:|------:|------:|------:|------:|------:|
|     16 |    7 |  251 |  865 | 1796 |  6691 |  5546 |  6265 |  9623 |  6077 |
|     17 |    7 |  270 | 1023 | 2004 |  6691 |  5799 |  7167 | 10780 |  6416 |
|     18 |    7 |  299 | 1088 | 2390 |  7581 |  6415 |  8576 | 11516 |  7310 |
|     19 |    7 |  348 | 1348 | 2948 |  7801 |  6844 |  9455 | 12699 |  7576 |
|     20 |    7 |  400 | 1492 | 3141 |  8840 |  7297 |  9874 | 12748 |  7576 |
|     21 |    7 |  429 | 1706 | 3172 |  9326 |  7558 | 10448 | 13617 |  8778 |
|     22 |    7 |  503 | 1766 | 4229 |  9984 |  7610 | 10448 | 14812 |  9142 |
|     23 |    7 |  503 | 1932 | 4747 | 10063 |  7764 | 11247 | 14812 |  9142 |
|     24 |   11 |  581 | 2144 | 5247 | 11369 | 10139 | 11431 | 17034 |  9852 |
|     25 |   11 |  592 | 2284 | 5346 | 12206 | 11115 | 12901 | 18400 | 10244 |

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

| month_short | month_long | 2018 | 2019 | 2020 | 2021 |  2022 |  2023 |  2024 |  2025 |  2026 |
|:------------|:-----------|-----:|-----:|-----:|-----:|------:|------:|------:|------:|------:|
| Jan         | January    |    7 |   45 |   94 |  224 |  1339 |  1180 |  1335 |  1098 |   933 |
| Feb         | February   |    7 |  133 |  277 |  532 |  1482 |  2953 |  2255 |  2056 |  2209 |
| Mar         | March      |    7 |  199 |  465 | 1363 |  4548 |  3793 |  5435 |  6642 |  4441 |
| Apr         | April      |    7 |  279 | 1023 | 2113 |  6691 |  5946 |  7567 | 11074 |  6416 |
| May         | May        |    7 |  429 | 1766 | 3740 |  9607 |  7610 | 10448 | 14812 |  9142 |
| Jun         | June       |   11 |  592 | 2493 | 5917 | 12747 | 11433 | 13489 | 19982 | 10244 |

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
