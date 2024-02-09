
# plotting theme from:

# get the fonts we need
library(showtext)

# load some new fonts
# font_add('fa-reg', 'fonts/Font Awesome 6 Free-Regular-400.otf')
# font_add('fa-brands', 'fonts/Font Awesome 6 Brands-Regular-400.otf')
# font_add('fa-solid', 'fonts/Font Awesome 6 Free-Solid-900.otf')

# add a gont from google fonts
font_add_google("Bebas Neue", "Bebas Neue")
font_add_google("Dosis", "Dosis")
font_add_google("Ubuntu", "Ubuntu")

# activate showtext
showtext_auto()

## ggplot theme
theme_set(theme_minimal(base_family = "Dosis", base_size = 12))

theme_update(
  plot.title = element_text(size = 18,
                            face = "bold",
                            hjust = .5,
                            margin = margin(5, 0, 5, 0)),
  plot.background = element_rect(fill = "#ede4d7", color = NA),
  panel.background = element_rect(fill = "#ede4d7", color = NA),
  panel.grid = element_blank(),
  panel.spacing.y = unit(0, "lines"),
  strip.text.y = element_blank(),
  legend.position = "bottom",
  legend.text = element_text(size = 9, color = "grey40"),
  legend.key.height = unit(0.5, "lines"),
  legend.key.width = unit(1, "lines"),
  plot.margin = margin(c(10, 10, 10, 10)),
  axis.line.y = element_line(color="grey40", size = 0.4),
  axis.ticks.y = element_line(colour = "grey40", size = 0.4),
  axis.line.x = element_line(color="grey40", size = 0.4),
  axis.ticks.x = element_line(colour = "grey40", size = 0.4)
)
