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
