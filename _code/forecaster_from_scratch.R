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
  geo_values = "*"
) |>
  select(geo_value, time_value, nssp = value)


empty_data <- tibble(time_value = seq(max()))

flu_data <- hhs_v_nhsn |>
  select(time_value, geo_value, hhs = old_source) |>
  left_join(nssp |> mutate(time_value = time_value + 6),
            by = join_by(geo_value, time_value)
  )

n_geos <- n_distinct(flu_data$geo_value)
max_time_value <- max(flu_data$time_value)
empty_data <- tibble(
  time_value = rep(max_time_value + days(1:3 * 7), each = n_geos),
  geo_value = rep(unique(flu_data$geo_value), times = 3),
  nssp = NA,
  hhs = NA
)

flu_data <- flu_data |>
  add_row(empty_data) |>
  mutate(epiweek = epiweek(time_value)) |>
  left_join(climate, by = join_by(geo_value, epiweek)) |>
  select(!epiweek) |>
  filter(geo_value %nin% c("as", "vi", "gu", "mp", "usa")) |>
  arrange(geo_value, time_value) |>
  as_epi_df()

r <- epi_recipe(flu_data) |>
  step_population_scaling(
    hhs, nssp,
    df = epidatasets::state_census,
    df_pop_col = "pop",
    create_new = FALSE,
    rate_rescaling = 1e5,
    by = c("geo_value" = "abbr")) |>
  step_mutate(hhs = hhs^(1/4), nssp = nssp^(1/4), climate_pred = climate_pred^(1/4)) |>
  step_epi_lag(hhs, lag = c(0, 7, 14)) |>
  step_epi_lag(nssp, lag = c(0, 7, 14)) |>
  step_epi_ahead(hhs, ahead = 14) |>
  step_epi_ahead(climate_pred, ahead = 14, role = "predictor") |>
  step_epi_naomit()

# Training engine
e <- quantile_reg(quantile_levels = c(0.01, 0.025, 1:19 / 20, 0.975, 0.99)) # 23 ForecastHub quantiles

# A post-processing routine describing what to do to the predictions
f <- frosting() |>
  layer_predict() |>
  layer_threshold(.pred, lower = 0)


# Bundle up the preprocessor, training engine, and postprocessor
# We use quantile regression
ewf <- epi_workflow(r, e, f)

# Fit it to data (we could fit this to ANY data that has the same format)
trained_ewf <- ewf |> fit(flu_data)

# we could make predictions using the same model on ANY test data
preds <- forecast(trained_ewf) |>
  left_join(epidatasets::state_census |> select(pop, abbr), join_by(geo_value == abbr)) |>
  mutate(
    .pred = .pred^4 * pop / 1e5,
    forecast_date = time_value,
    target_date = forecast_date + days(14),
    time_value = NULL,
    pop = NULL
  )
