## ======================= Load Required Packages ==============================

packages <- c(
  'FITfileR',    # read FIT files
  'tidyverse',   # data cleaning/visualization
  'ggmap',       # obtain Google map imagery
  'ggwordcloud', # plot names
  'ggtext',      # markdown text on ggplots
  'showtext'     # import Google fonts for plots
  )

lapply(packages, library, character.only = TRUE)


## ================= Get Google API Key/Register Account =======================

API_KEY <- Sys.getenv("API_KEY")

register_google(API_KEY,
                account_type = "standard",
                day_limit = 2500)


## ======================= Styling Map Interface ===============================

style1 = c(element = "geometry", color = "0x242f3e")
style2 = c("&style=", element = "labels.text.fill", color = "0x746855")
style3 = c("&style=", element = "labels.text.stroke", color = "0x242f3e")
style4 = c("&style=", feature = "adminsitrative.land_parcel", element = "labels", visibility = "off")
style5 = c("&style=", feature = "administrative.locality", element = "labels.test.fill", color = "0xd59563")
style22 = c("&style=", feature = "administrative.neighborhood", element = "labels", visibility = "off")
style6 = c("&style=", feature = "poi", element = "labels.text", visibility = "off")
style7 = c("&style=", feature = "poi", element = "labels.text.fill", color = "0xd59563")
style8 = c("&style=", feature = "poi", element = "geometry", color = "0x263c3f")
style9 = c("&style=", feature = "poi.park", element = "labels", visibility = "off")
style21 = c("&style=", feature = "poi.business", element = "labels", visibility = "off")
style10 = c("&style=", feature = "road", element = "geometry", color = "0x38414e")
style11 = c("&style=", feature = "road", element = "geometry.stroke", color = "0x212a37")
style12 = c("&style=", feature = "road", element = "labels.text.fill", color = "0x9ca5b3")
style13 = c("&style=", feature = "road.highway", element = "geometry", color = "0x746855")
style14 = c("&style=", feature = "road.highway", element = "geometry.stroke", color = "0x1f2835")
style15 = c("&style=", feature = "road.highway", element = "labels.text.fill", color = "0xf3d19c")
style16 = c("&style=", feature = "road.local", element = "labels", visibility = "off")
style17 = c("&style=", feature = "transit", element = "geometry", color = "0x2f3948")
style17 = c("&style=", feature = "transit.station", element = "labels.text.fill", color = "0xd59563")
style18 = c("&style=", feature = "water", element = "geometry", color = "0x17263c")
style19 = c("&style=", feature = "water", element = "labels.text.fill", color = "0x515c6d")
style20 = c("&style=", feature = "water", element = "labels.text.stroke", color = "0x17263c")
style23 = c("&style=", element = "labels", visibility = "off")

dark_style = c(style1, style2, style3, style4, style5, style6, style7, style8, style9, style10, style11, style12, style13, style14, style15, style16, style17, style18, style19, style20, style21, style22, style23)


## ========================= Create ggplot Theme ===============================

font_add_google("Rajdhani")
font_add(family = "fa-brands", regular = "src/fonts/Font Awesome 6 Brands-Regular-400.otf")
github_icon <- paste0("<span style='font-family:fa-brands;color:black'>&#xf09b;</span>")

FONT_FAMILY <- "Rajdhani"

showtext_auto()

plot_caption <- paste0( github_icon, " @jedjohnson4")

HIGHLIGHT_COLOR <- "#0f3610"

my_theme <- theme(
  axis.title = element_text(family = FONT_FAMILY,
                            size = 70,
                            color = "black"),
  axis.text = element_text(family = FONT_FAMILY,
                           size = 60,
                           color = "black"),
  panel.grid.major.y = element_line(color = "gray80"),
  axis.title.x = element_text(margin = margin(t = 5)),
  axis.title.y = element_text(angle = 90,
                              margin = margin(r = 5)),
  axis.text.y = element_text(hjust = 1,
                             margin = margin(r = 2)),
  axis.text.x = element_text(margin = margin(t = 1)),
  plot.background = element_rect(fill = "white",
                                 color = NA),
  plot.title = element_markdown(family = FONT_FAMILY,
                                size = 120,
                                margin = margin(b = 3)),
  plot.subtitle = element_markdown(family = FONT_FAMILY,
                                   size = 70,
                                   margin = margin(b = 5)),
  plot.caption = element_markdown(family = FONT_FAMILY,
                                   size = 60),
  plot.margin = unit(c(10,10,5,5), "pt")
)
