file_path <- here::here("_data/ca_archive.rds")

if (!file.exists(file_path)) {
  states <- "ca"

  cases_archive <- pub_covidcast(
    source = "jhu-csse",
    signals = "confirmed_incidence_prop",
    time_type = "day",
    geo_type = "state",
    time_values = epirange(20200401, 20230401),
    geo_values = states,
    issues = epirange(20200401, 20230401)) %>%
    select(geo_value, time_value, version = issue, case_rate = value) %>%
    arrange(geo_value, time_value) %>%
    as_epi_archive(compactify = FALSE)

  deaths_archive <- pub_covidcast(
    source = "jhu-csse",
    signals = "deaths_incidence_prop",
    time_type = "day",
    geo_type = "state",
    time_values = epirange(20200401, 20230401),
    geo_values = states,
    issues = epirange(20200401, 20230401)) %>%
    select(geo_value, time_value, version = issue, death_rate = value) %>%
    arrange(geo_value, time_value) %>%
    as_epi_archive(compactify = FALSE)

  ca_archive <- epix_merge(cases_archive, deaths_archive, sync = "locf")
  saveRDS(ca_archive, file = file_path)
} else {
  ca_archive <- readRDS(file_path)
}
