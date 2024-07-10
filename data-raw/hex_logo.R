library(hexSticker)

logo_image <- fs::path("data-raw", "forestables_logo_image.png")
sticker(
  logo_image,
  package = "forestables", p_size = 18, p_y = 1.35, p_color = "#BFD77A",
  s_x = 1, s_y = .9, s_width = .6,
  filename = fs::path("data-raw", "forestables.png"),
#   url = "emf.creaf.cat", u_size = 6, u_color = "#BFD77A", u_y = .2, u_x = 1.2,
  h_fill = "#41B6B6", h_color = "#BFD77A"
)
