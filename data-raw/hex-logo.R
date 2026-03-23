library(hexSticker)

sticker(
  "data-raw/anon-robot.svg",
  package = "",
  p_size = 0,
  p_y = 0.40,
  s_x = 1,
  s_y = 0.98,
  s_width = 0.85,
  s_height = 0.85,
  h_fill = "#1A1D21",
  h_color = "#b9c2ca",
  h_size = 1.4,
  filename = "man/figures/logo.png",
  dpi = 300
)

message("Logo saved to man/figures/logo.png")

pkgdown::build_favicons(".", overwrite = TRUE)
