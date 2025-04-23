options(htmltools.dir.version = FALSE)
primary_orig <- "#002145"
primary <- "#003671" # 10% lighter
secondary <- "#a41623"
tertiary <- "#f9c80e"
fourth_colour <- "#177245"
fifth_colour <- "#A393BF"
ubclblue <- "#6495ed"
theme_white <- "#fbfcff"
theme_black <- "#00162e"
colvec <- c(
  primary = primary, secondary = secondary,
  tertiary = tertiary, fourth_colour = fourth_colour,
  fifth_colour = fifth_colour, ubclblue = ubclblue
)
library(epiprocess)
suppressMessages(library(tidyverse))
ggplot2::theme_set(ggplot2::theme_bw())
theme_update(legend.position = "bottom", legend.title = element_blank())
delphi_pal <- function(n) {
  if (n > 6L) warning("Not enough colors in this palette!")
  unname(colvec)[1:n]
}
scale_fill_delphi <- function(..., aesthetics = "fill") {
  discrete_scale(aesthetics = aesthetics, palette = delphi_pal, ...)
}
scale_color_delphi <- function(..., aesthetics = "color") {
  discrete_scale(aesthetics = aesthetics, palette = delphi_pal, ...)
}
scale_colour_delphi <- scale_color_delphi

options(
  asciicast_theme = list(
    black         = c(grDevices::col2rgb("#073642")),
    red           = c(grDevices::col2rgb("#a41623")),
    green         = c(grDevices::col2rgb("#177245")),
    yellow        = c(grDevices::col2rgb("#FFa319")),
    blue          = c(grDevices::col2rgb("#6495ed")),
    magenta       = c(grDevices::col2rgb("#d33682")),
    cyan          = c(grDevices::col2rgb("#2aa198")),
    white         = c(grDevices::col2rgb("#eee8d5")),
    light_black   = c(grDevices::col2rgb("#002b36")),
    light_red     = c(grDevices::col2rgb("#cb4b16")),
    light_green   = c(grDevices::col2rgb("#586e75")),
    light_yellow  = c(grDevices::col2rgb("#f9c80e")),
    light_blue    = c(grDevices::col2rgb("#17bebb")),
    light_magenta = c(grDevices::col2rgb("#6c71c4")),
    light_cyan    = c(grDevices::col2rgb("#93a1a1")),
    light_white   = c(grDevices::col2rgb("#fdf6e3")),
    background    = c(grDevices::col2rgb("#ffffff")),
    cursor        = c(grDevices::col2rgb("#00162e")),
    bold          = c(grDevices::col2rgb("#00162e")),
    text          = c(grDevices::col2rgb("#00162e"))
  )
)


qrdat <- function(text, ecl = c("L", "M", "Q", "H")) {
  x <- qrcode::qr_code(text, ecl)
  n <- nrow(x)
  s <- seq_len(n)
  tib <- tidyr::expand_grid(x = s, y = rev(s))
  tib$z <- c(x)
  tib
}

wkshtqr <- qrdat("https://dajmcdon.github.io/canssip-epidata-2025/worksheet") |>
  ggplot(aes(x, y, fill = z, alpha = z)) +
  geom_raster() +
  coord_equal(expand = FALSE) +
  scale_fill_manual(values = c("white", primary), guide = "none") +
  scale_alpha_manual(values = c(0, 1), guide = "none") +
  theme_void() +
  theme(
    text = element_text(
      color = primary, size = 36,
      margin = margin(3,0,3,0))
  )
