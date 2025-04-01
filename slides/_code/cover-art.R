source(here::here("slides", "_code", "setup.R"))
x <- archive_cases_dv_subset
x_latest <- epix_as_of(x, version = max(x$DT$version))
self_max = max(x$DT$version)
versions = seq(as.Date("2020-06-01"), self_max - 1, by = "2 weeks")
snapshots_all <- map_dfr(versions, function(v) {
  epix_as_of(x, version = v) %>% mutate(version = v)}) %>%
  bind_rows(x_latest %>% mutate(version = self_max)) %>%
  mutate(latest = version == self_max)
snapshots <- snapshots_all %>%
  filter(geo_value %in% c("ca", "fl"))

ca <- snapshots_all |>
  arrange(geo_value, version, time_value) |>
  ggplot(aes(x = time_value, y = percent_cli)) +
  geom_line(
    aes(
      color = scale(as.numeric(interaction(version, geo_value))),
      group = interaction(geo_value, version)
    )
  ) +
  scale_x_date(minor_breaks = "month", labels = NULL, expand = expansion()) +
  labs(x = "", y = "") +
  scale_y_continuous(expand = expansion(c(0, 0.05))) +
  theme_void() +
  scale_color_distiller(palette = "Set1", direction = 1) +
  theme(legend.position = "none", panel.background = element_blank())

ggsave(
  "cover-art-1.svg",
  path = here::here("slides", "gfx"),
  bg = primary,
  width = 8,
  height = 4.5
)
