library(tidyverse)
library(epidatr)
library(epiprocess)
library(epipredict)


cases_path <- here::here("_data/cases.rds")

if (!file.exists(cases_path)) {
  cases <- pub_covidcast(
    source = "jhu-csse",
    signals = "confirmed_incidence_num",
    time_type = "day",
    geo_type = "state",
    time_values = epirange(20200401, 20230401),
    geo_values = "*") %>%
    select(geo_value, time_value, cases = value)
  saveRDS(cases, file = cases_path)
} else {
  cases <- readRDS(cases_path)
}

deaths_path <- here::here("_data/deaths.rds")

if (!file.exists(deaths_path)) {
  deaths <- pub_covidcast(
    source = "jhu-csse",
    signals = "deaths_incidence_num",
    time_type = "day",
    geo_type = "state",
    time_values = epirange(20200401, 20230401),
    geo_values = "*") %>%
    select(geo_value, time_value, deaths = value)
  saveRDS(deaths, file = deaths_path)
} else {
  deaths <- readRDS(deaths_path)
}
