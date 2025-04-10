# Last ran on Apr 2 2025
# Modifications of epidatasets/data-raw/can_prov_cases_tbl.R

library(dplyr)
library(readr)
library(purrr)
library(httr)
library(jsonlite)

gh_token <- gh::gh_token()
auth_header <- httr::add_headers(Authorization = paste("Bearer", gh_token))

BASE_URL <- "https://api.github.com/repos/ccodwg/CovidTimelineCanada/commits?sha=%s&per_page=%s&path=data/pt/cases_pt.csv&until=%s&page=%s"
ITEMS_PER_PAGE <- 100
BRANCH <- "main"

since_date <- strftime("2024-05-01", "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

page <- 0
commit_pages <- list()

# Fetch list of commits from API, one page at a time. Each page contains up to
# 100 commits. If a page contains 100 commits, assume that there are more
# results and fetch the next page.
while (page == 0 || nrow(commit_page) == 100) {
  page <- page + 1
  # Construct the URL
  commits_url <- sprintf(BASE_URL, BRANCH, ITEMS_PER_PAGE, since_date, page)

  request <- GET(commits_url, auth_header)
  # Convert any HTTP errors to R errors automatically.
  stop_for_status(request)

  # Convert results from nested JSON/list to dataframe. If no results returned,
  # `commit_page` will be an empty list.
  commit_page <- content(request, as = "text") %>%
    fromJSON(simplifyDataFrame = TRUE, flatten = TRUE) %>%
    # Trim message down a bit.
    mutate(message = substr(commit.message, 1, 40)) %>%
    select(sha, url = commit.url, message)

  # No more results are being returned.
  if (identical(commit_page, list())) {
    break
  }

  commit_pages[[page]] <- commit_page
}

# Combine all requested pages of commits into one dataframe
commit_pages <- bind_rows(commit_pages)

BASE_DATA_URL <- "https://raw.githubusercontent.com/ccodwg/CovidTimelineCanada/%s/data/pt/cases_pt.csv"

commit_pages <- mutate(
  commit_pages,
  data_url = sprintf(BASE_DATA_URL, sha),
  date = strsplit(message, " ") %>% map_chr(~ substr(.x[3], start=1, stop=10)) %>% as.Date()
) %>%
  na.omit()

ca_pop_url <- "https://raw.githubusercontent.com/mountainMath/BCCovidSnippets/main/data/prov_pop.csv"
ca_pop <- read_csv(
  ca_pop_url,
  col_types = cols(
    Province = col_character(),
    shortProvince = col_character(),
    Population = col_integer()
  )
) %>%
  rename(province = Province, abbreviation = shortProvince, population = Population)
abbrev_map <- setNames(ca_pop$province, ca_pop$abbreviation)

# Read in data
can_prov_cases_tbl <- purrr::map2(commit_pages$data_url, commit_pages$date, function(url, date) {
  raw <- readr::read_csv(
    url,
    col_types = cols_only(
      region = col_character(),
      date = col_character(),
      value_daily = col_double()
    )
  )

  # Raw data uses a mix of full names and abbreviations. Switch to using only full names.
  raw$region <- case_when(
    raw$region == "NWT" ~ abbrev_map["NT"],
    raw$region == "PEI" ~ abbrev_map["PE"],
    raw$region %in% ca_pop$province ~ raw$region,
    raw$region %in% ca_pop$abbreviation ~ abbrev_map[raw$region],
    # Mark everything else as missing. Only applies to "Repatriated" region.
    TRUE ~ NA
  )

  raw %>%
    mutate(time_value = lubridate::ymd(date)) |>
    rename(geo_value = region, cases = value_daily) |>
    filter(!is.na(geo_value), time_value > "2020-01-01") |>
    select(geo_value, time_value, cases)
})

names(can_prov_cases_tbl) <- commit_pages$date
can_prov_cases_tbl <- can_prov_cases_tbl %>%
  bind_rows(.id = "version") %>%
  mutate(version = lubridate::ymd(version)) %>%
  arrange(version) %>%
  as_tibble()

can_prov_cases_tbl <- can_prov_cases_tbl |>
  distinct() |>
  group_by(geo_value, time_value, version) |>
  summarise(cases = max(cases)) |>
  ungroup()

arch <- epiprocess::as_epi_archive(can_prov_cases_tbl)
write_rds(arch$DT, file = here::here("_data", "can_cases_after_2022_archive.rds"))
