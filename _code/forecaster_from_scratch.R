## climate feature
roll_modular_multivec <- function(col, index, window_size, modulus = 53) {
  tib <- tibble(col = col, index = index) |>
    arrange(index) |>
    tidyr::nest(data = col, .by = index)
  out <- double(nrow(tib))
  for (iter in seq_along(out)) {
    entries <- (iter - window_size):(iter + window_size) %% modulus
    entries[entries == 0] <- modulus
    out[iter] <- with(
      purrr::list_rbind(tib$data[entries]),
      median(col, na.rm = TRUE)
    )
  }
  tibble(index = unique(tib$index), climate_pred = out)
}

climatological_feature <- function(epi_data, window_size = 3) {
  epi_data |>
    filter(
      (season != "2020/21") & (season != "2021/22"), # drop weird years
    ) |>
    select(nhsn, epiweek, geo_value) |>
    reframe(roll_modular_multivec(nhsn, epiweek, window_size, 53), .by = geo_value) |>
    mutate(climate_pred = pmax(0, climate_pred)) |>
    rename(epiweek = index)
}

climate <- climatological_feature(climate_data |> select(nhsn, epiweek, season, geo_value))

## get exogenous feature

nssp <- pub_covidcast(
  source = "nssp",
  signal = "pct_ed_visits_influenza",
  time_type = "week",
  geo_type = "state",
  geo_values = "*")
