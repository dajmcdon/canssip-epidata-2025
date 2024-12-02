library(tidyverse)
library(epidatr)
library(epiprocess)
library(epipredict)


hhs_path <- here::here("_data/hospitalizations.rds")

if (!file.exists(hhs_path)) {
  hhs <- pub_covidcast(
    source = "hhs",
    signals = "confirmed_admissions_covid_1d_prop_7dav",
    time_type = "day",
    geo_type = "state",
    time_values = epirange(20200401, 20230401),
    geo_values = "*") |>
    select(geo_value, time_value, hospitalizations = value)
  saveRDS(hhs, file = hhs_path)
} else {
  hhs <- readRDS(hhs_path)
}

