#' ---
#' title: 'Data processing: Small boat activity in the English Channel'
#' date-format: long
#' date: last-modified
#' lang: en
#' format:
#'   gfm:
#'     html-math-method: katex
#'     fig-width: 15
#'     fig-asp: 1
#'     code-annotations: below
#'     df-print: kable
#'     wrap: none
#' execute:
#'   echo: true
#'   eval: true
#'   warning: false
#' ---
#'
#' by [Isaac Arroyo](https://github.com/isaacarroyov), Data Visualisation Journalist

#| label: load-libraries-paths
here::i_am("processing/processing.R")

library(tidyverse)

path2repo <- here::here()
path2input_data <- here::here("input_data")
path2output_data <- here::here("output_data")

#' ## Intro
#' 
#' Data on migrants arriving in the UK in small boats is published weekly, 
#' every Friday, by the UK's [Home 
#' Office](https://www.gov.uk/government/organisations/home-office) and 
#' [Border Force](https://www.gov.uk/government/organisations/border-force).
#' 
#' [Click here to visit the website and download the official data on 
#' Migrants detected crossing the English Channel in small 
#' boats](https://www.gov.uk/government/publications/migrants-detected-crossing-the-english-channel-in-small-boats).
#' 
#' This repository serves as an open access to the data, including 
#' additional statistics such as cumulative figures and the ratio of 
#' migrants per boat.
#' 
#' For any questions about the data, please contact 
#' migrationstatsenquiries@homeoffice.gov.uk; and for any questions on the 
#' processing or code, raise an issue or contact datavis@thenationalnews.com
#' 
#' > [!NOTE]
#' >
#' > The data published here is provisional and subject to change, 
#' including reduction. Finalised data on small boat crossings since 2018 
#' is published in the quarterly Immigration system statistics under the 
#' topic "How many people come to the UK irregularly".
#' 
#' The script creates 7 CSVs:
#' 
#' * Long format:
#'   - Daily arrivals of small boats and migrants
#'   - Weeekly arrivals of small boats and migrants
#'   - Monthly arrivals of small boats and migrants
#'   - Yearly arrivals of small boats and migrants
#' * Wide format:
#'   - Weeekly cumulative arrivals of migrants per year
#'   - Monthly cumulative arrivals of migrants per year

#| label: load_data-source
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

#'

#| label: see_last_ten_rows
#| echo: false
slice_tail(data_source, n = 10)

#' > [!NOTE]
#' >
#' > For the rest of the script, the columns to use are 
#' `full_date`, `migrants_arrived` and `boats_arrived`
#' 
#' ## Long format
#' 
#' Besides the date components (`n_year`, `n_month`, `n_day`, `n_week`, and 
#' `date_label`), the datasets will have three more columns:
#' 
#' - Ratio of migrants per boat (`migrants_per_boat`): All
#' - Cumulative number of migrant arrivals (`cumulative_migrants_arrived`): Except `db_yearly_migrants_boats`
#' - Cumulative number of small boats arrivals (`cumulative_boats_arrived`): Except `db_yearly_migrants_boats`
#' 
#' ### Daily migrant and boats arrivals

#| label: create-db_daily_migrants_boats
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

#' **Daily migrants and small boats arrivals** (last ten rows)

#| label: see_last_rows-db_daily_migrants_boats
#| echo: false
slice_tail(db_daily_migrants_boats, n = 10)

#' ### Weekly migrant and boat arrivals
#' 
#' In the database provided by the UK government there is a sheet with the 
#' weekly record of migrant arrivals, however the cutoff is at the end of 
#' that week. The problem with recording or grouping the data in this way 
#' is that week 53 exists in a leap year, or when the beginning and end of 
#' a year share a week, whether the first or the last. This temporal offset 
#' also causes a discrepancy in the final numbers.
#' 
#' As a solution, the following is done:
#' 
#' * Label the first days of January as week 1 if they fall in week 52 or 53
#' * Label the last days of December as week 52 if they fall in week 1 or 53
#' 
#' In this way, the end-of-year figures remain consistent across all the datasets created in this script.

#| label: create-db_weekly_migrants_boats
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

#' 

#| label: see_last_rows-db_weekly_migrants_boats
#| echo: false
slice_tail(db_weekly_migrants_boats, n = 10)

#' ### Monthly migrant and boat arrivals

#| label: create-db_monthly_migrants_boats
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

#' 

#| label: see_last_rows-db_monthly_migrants_boats
#| echo: false
slice_tail(db_monthly_migrants_boats, n = 10)

#' ### Yearly migrant and boat arrivals

#| label: create-db_yearly_migrants_boats
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

#' 

#| label: see_last_rows-db_yearly_migrants_boats
#| echo: false
slice_tail(db_yearly_migrants_boats, n = 10)

#' ## Wide format
#' 
#' **Weekly cumulative migrant arrivals**

#| label: create-wide_weekly_cumulative_migrants
wide_weekly_cumulative_migrants <- db_weekly_migrants_boats %>%
  select(n_year, n_week, cumulative_migrants_arrived) %>%
  pivot_wider(
    names_from = n_year,
    values_from = cumulative_migrants_arrived)

#' 

#| label: view_sample-wide_weekly_cumulative_migrants
#| echo: false
last_column <- colnames(wide_weekly_cumulative_migrants)[length(colnames(wide_weekly_cumulative_migrants))]
slice_tail(wide_weekly_cumulative_migrants %>% filter(!is.na(!!sym(last_column))), n = 10)

#' **Monhtly cumulative migrant arrivals**

#| label: create-wide_monthly_cumulative_migrants
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

#'

#| label: view_sample-wide_monthly_cumulative_migrants
#| echo: false
last_column <- colnames(wide_monthly_cumulative_migrants)[length(colnames(wide_monthly_cumulative_migrants))]
slice_tail(wide_monthly_cumulative_migrants %>% filter(!is.na(!!sym(last_column))), n = 10)

#' ## Save data
#' 
#' 
#' ### ... long format

#| label: save_data-long_format
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

#' ### ... wide format

#| label: save_data-wide_format
write_csv(
  x = wide_weekly_cumulative_migrants,
  file = here::here(path2output_data, "wide_uk_small_boats_weekly_cumulative_migrants.csv"),
  na = "")

write_csv(
  x = wide_monthly_cumulative_migrants,
  file = here::here(path2output_data, "wide_uk_small_boats_monthly_cumulative_migrants.csv"),
  na = "")
