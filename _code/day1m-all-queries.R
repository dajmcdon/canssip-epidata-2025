library(tidyverse)
library(epidatr)
library(epiprocess)

enlist <- function(...) {
  rlang::dots_list(..., .homonyms = "error", .named = TRUE, .check_assign = TRUE)
}


# -------------------------------------------------------------------------

dv_wa <- pub_covidcast(
  source = "doctor-visits",
  signals = "smoothed_adj_cli",
  geo_type = "state",
  time_type = "day",
  geo_values = "wa", # Just for WA to keep it simple (& to go with the case data by test date for that state)
  time_value = epirange(20211201, 20220201),
  issues = epirange(20211201, 20220201)
) |>
  select(geo_value, version = issue, time_value, percent_cli = value) |>
  as_epi_archive(compactify = FALSE)

# -------------------------------------------------------------------------



panel_sources <- c("jhu-csse", "chng", "chng", "hhs")
panel_signals <- c("confirmed_7dav_incidence_prop",
                   "smoothed_adj_outpatient_cli",
                   "smoothed_adj_outpatient_covid",
                   "confirmed_admissions_covid_1d_prop_7dav")

panel_data <- map2(panel_sources, panel_signals, ~ pub_covidcast(
  .x, .y, geo_type = "state", geo_values = "ca,fl,nc,wa",
  time_type = "day",
  time_values = epirange(20210915, 20220915)) |>
    select(geo_value, time_value, source, value)
)


# -------------------------------------------------------------------------

dv_versioned_panel <- pub_covidcast(
  source = "doctor-visits",
  signals = "smoothed_adj_cli",
  geo_type = "state",
  geo_values = "fl,ca,ny,tx",
  time_type = "day",
  time_values = epirange(20200601, 20211201),
  issues = epirange(20200601, 20211201)
) |>
  select(time_value, geo_value, percent_cli = value, version = issue)

dv_versioned_panel_final <- pub_covidcast(
  source = "doctor-visits",
  signals = "smoothed_adj_cli",
  geo_type = "state",
  geo_values = "fl,ca,ny,tx",
  time_type = "day",
  time_values = epirange(20200601, 20211201)
) |>
  select(time_value, geo_value, percent_cli = value, version = issue)


# -------------------------------------------------------------------------
dv_wa_versioned <- pub_covidcast(
  source = "doctor-visits",
  signals = "smoothed_cli",
  geo_type = "state",
  time_type = "day",
  geo_values = "wa",
  time_values = epirange(20211201, 20220201),
  issues = epirange(20211201, 20220201)
) |>
  select(time_value, value, issue)

dv_wa_finalized <- pub_covidcast(
  source = "doctor-visits",
  signals = "smoothed_cli",
  geo_type = "state",
  time_type = "day",
  geo_values = "wa",
  time_values = epirange(20211201, 20220201),
  as_of = "2022-02-01"
) |>
  select(time_value, final_value = value)



# -------------------------------------------------------------------------

as_ofs <- seq(as.Date("2020-09-28"), as.Date("2020-10-19"), by = "week")
cases_311_as_of <- map_dfr(as_ofs, function(as_of) {
  pub_covidcast(source = "jhu-csse",
                signals = "confirmed_7dav_incidence_prop",
                geo_type = "hrr",
                time_type = "day",
                geo_values = "311",
                as_of = as_of,
                time_values = epirange(20200815, 20200926)) |>
    mutate(as_of = as_of)
})
dv_311_as_of <-  map_dfr(as_ofs, function(as_of) {
  pub_covidcast(source = "doctor-visits",
                signals = "smoothed_adj_cli", # Estimated % of outpatient doctor visits primarily about COVID-related symptoms
                geo_type = "hrr",
                time_type = "day",
                geo_values = "311",
                as_of = as_of,
                time_values = epirange(20200815, 20200926)) |>
    mutate(as_of = as_of)
})


# -------------------------------------------------------------------------

sources <- c("jhu-csse", "fb-survey", "doctor-visits", "google-symptoms", "chng")
signals <- c("confirmed_7dav_incidence_num", "smoothed_whh_cmnty_cli",
             "smoothed_adj_cli", "sum_anosmia_ageusia_smoothed_search",
             "smoothed_adj_outpatient_cli")
reinhart <- map2(sources, signals, ~ pub_covidcast(
  .x, .y, time_type = "day", geo_type = "county",
  time_values = epirange(20200615, 20200815),
  geo_values = "48029") |>
    select(source, time_value, value)
)

# -------------------------------------------------------------------------


hhs_flu_nc <- pub_covidcast(
  'hhs', 'confirmed_admissions_influenza_1d', 'state', 'day',
  geo_values = 'nc',
  time_values = c(20240401, 20240405:20240414)
)


# -------------------------------------------------------------------------


jhu_us_cases <- pub_covidcast(
  source = "jhu-csse",
  signals = "confirmed_7dav_incidence_prop",
  geo_type = "nation",
  time_type = "day",
  geo_values = "us",
  time_values = epirange(20210101, 20210401)
)


# -------------------------------------------------------------------------


jhu_state_cases <- pub_covidcast(
  source = "jhu-csse",
  signals = "confirmed_7dav_incidence_prop",
  geo_type = "state",
  time_type = "day",
  geo_values = "*",
  time_values = epirange(20210101, 20210401)
)


# -------------------------------------------------------------------------

jhu_county_cases <- pub_covidcast(
  source = "jhu-csse",
  signals = "confirmed_7dav_incidence_prop",
  geo_type = "county",
  time_type = "day",
  time_values = epirange(20210101, 20210401),
  geo_values = "06059"
)


# -------------------------------------------------------------------------

dv_pa_as_of <- pub_covidcast(
  source = "doctor-visits",
  signals = "smoothed_adj_cli",
  time_type = "day",
  time_values = epirange("2020-05-01", "2020-05-01"),
  geo_type = "state",
  geo_values = "pa",
  as_of = "2020-05-07"
)


# -------------------------------------------------------------------------


dv_pa_final <- pub_covidcast(
  source = "doctor-visits",
  signals = "smoothed_adj_cli",
  time_type = "day",
  time_values = epirange("2020-05-01", "2020-05-01"),
  geo_type = "state",
  geo_values = "pa"
)


# -------------------------------------------------------------------------

dv_pa_issues <- pub_covidcast(
  source = "doctor-visits",
  signals = "smoothed_adj_cli",
  time_type = "day",
  time_values = epirange("2020-05-01", "2020-05-01"),
  geo_type = "state",
  geo_values = "pa",
  issues = epirange("2020-05-01", "2020-05-15")
)


# -------------------------------------------------------------------------

dv_pa_issues_sub <- pub_covidcast(
  source = "doctor-visits",
  signals = "smoothed_adj_cli",
  time_type = "day",
  time_values = epirange("2020-05-01", "2020-05-01"),
  geo_type = "state",
  geo_values = "pa",
  issues = epirange("1900-01-01", "2020-05-15")
)


# -------------------------------------------------------------------------


dv_pa_issues_all <- pub_covidcast(
  source = "doctor-visits",
  signals = "smoothed_adj_cli",
  time_type = "day",
  time_values = epirange("2020-05-01", "2020-05-01"),
  geo_type = "state",
  geo_values = "pa",
  issues = epirange("1900-01-01", "2024-12-11") # From the 1900s to today
)


# -------------------------------------------------------------------------

dv_pa_issues_star <- pub_covidcast(
  source = "doctor-visits",
  signals = "smoothed_adj_cli",
  time_type = "day",
  time_values = epirange("2020-05-01", "2020-05-07"),
  geo_type = "state",
  geo_values = "pa",
  issues = "*"
)


# -------------------------------------------------------------------------

dv_state_issues_star <- pub_covidcast(
  source = "doctor-visits",
  signals = "smoothed_adj_cli",
  time_type = "day",
  time_values = epirange("2020-05-01", "2020-05-07"),
  geo_type = "state",
  geo_values = "*",
  issues = "*"
)


# -------------------------------------------------------------------------

dv_state_default <- pub_covidcast(
  source = "doctor-visits",
  signals = "smoothed_adj_cli",
  time_type = "day",
  time_values = epirange("2020-05-01", "2020-05-07"),
  geo_type = "state"
)

# -------------------------------------------------------------------------

save(
  cases_311_as_of, dv_311_as_of, dv_pa_as_of, dv_pa_final, dv_pa_issues,
  dv_pa_issues_all, dv_pa_issues_star, dv_pa_issues_sub, dv_state_default,
  dv_state_issues_star, dv_versioned_panel, dv_versioned_panel_final,
  dv_wa, dv_wa_finalized, dv_wa_versioned, hhs_flu_nc, jhu_county_cases,
  jhu_state_cases, jhu_us_cases, panel_data, reinhart,
  file = here::here("_data", "day1m_queries.rda"),
  compress = TRUE
)
