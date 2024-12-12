library(epiprocess)

hhs_arch <- read_csv("https://healthdata.gov/resource/g62h-syeh.csv?$limit=90000&$select=date,state,previous_day_admission_influenza_confirmed") %>%
  mutate(
    geo_value = tolower(state),
    time_value = as.Date(date) - 1L,
    hhs = previous_day_admission_influenza_confirmed
  ) |>
  select(geo_value, time_value, hhs) |>
  as_epi_df() |>
  group_by(geo_value) |>
  epi_slide_sum(
    hhs,
    na.rm = TRUE,
    .window_size = 7L,
    .ref_time_values = seq.Date(as.Date("2020-01-04"), as.Date("2024-04-24"), by = 7),
  ) |>
  mutate(hhs = hhs_7dsum, hhs_7dsum = NULL)

convert_epiweek_to_season <- function(epiyear, epiweek) {
  # Convert epiweek to season
  update_inds <- epiweek <= 39
  epiyear <- ifelse(update_inds, epiyear - 1, epiyear)

  season <- paste0(epiyear, "/", substr((epiyear + 1), 3, 4))
  return(season)
}

epiweeks_in_year <- function(year) {
  last_week_of_year <- seq.Date(as.Date(paste0(year, "-12-24")),
                                as.Date(paste0(year, "-12-31")),
                                by = 1
  )
  return(max(as.numeric(MMWRweek::MMWRweek(last_week_of_year)$MMWRweek)))
}

convert_epiweek_to_season_week <- function(epiyear, epiweek, season_start = 39) {
  season_week <- epiweek - 39
  update_inds <- season_week <= 0
  if (!any(update_inds)) {
    # none need to be updated
    return(season_week)
  }
  # last year's # of epiweeks determines which week in the season we're at at
  # the beginning of the year
  season_week[update_inds] <- season_week[update_inds] +
    sapply(epiyear[update_inds] - 1, epiweeks_in_year)

  return(season_week)
}

df <- readr::read_csv("https://data.cdc.gov/resource/ua7e-t2fy.csv?$limit=20000&$select=weekendingdate,jurisdiction,totalconfflunewadm")
df <- df %>%
  mutate(
    epiweek = epiweek(weekendingdate),
    epiyear = epiyear(weekendingdate)
  ) %>%
  left_join(
    (.) %>%
      distinct(epiweek, epiyear) %>%
      mutate(
        season = convert_epiweek_to_season(epiyear, epiweek),
        season_week = convert_epiweek_to_season_week(epiyear, epiweek)
      ),
    by = c("epiweek", "epiyear")
  )


to_compare <- df %>%
  mutate(time_value = as.Date(weekendingdate), geo_value = tolower(jurisdiction), nhsn = totalconfflunewadm) %>%
  select(-weekendingdate, -jurisdiction, -totalconfflunewadm) %>%
  full_join(hhs_arch, by = join_by(geo_value, time_value)) %>%
  select(time_value, geo_value, old_source = hhs, new_source = nhsn)

saveRDS(
  df |>
    mutate(time_value = as.Date(weekendingdate), geo_value = tolower(jurisdiction), nhsn = totalconfflunewadm) %>%
    select(-weekendingdate, -jurisdiction, -totalconfflunewadm),
  here::here("_data", "climatological_model_data.rds")
)
saveRDS(to_compare, here::here("_data", "hhs_v_nhsn.rds"))

