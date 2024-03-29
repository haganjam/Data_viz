
# plotting theme from:

# get the fonts we need

# load some new fonts
font_add('fa-reg', 'fonts/Font Awesome 6 Free-Regular-400.otf')
font_add('fa-brands', 'fonts/Font Awesome 6 Brands-Regular-400.otf')
font_add('fa-solid', 'fonts/Font Awesome 6 Free-Solid-900.otf')

# add a gont from google fonts
font_add_google("Bebas Neue", "Bebas Neue")
font_add_google("Libre Caslon Text", "Caslon")
font_add_google("Ubuntu", "Ubuntu")

# activate showtext
showtext_auto()

## ggplot theme
theme_set(theme_minimal(base_family = "Ubuntu", base_size = 12))

theme_update(
  plot.title = element_text(size = 18,
                            face = "bold",
                            hjust = .5,
                            margin = margin(5, 0, 5, 0)),
  plot.caption = element_text(size = 9,
                              color = "grey40",
                              hjust = .5,
                              margin = margin(20, 0, 5, 0)),
  plot.background = element_rect(fill = "#f0e9df", color = NA),
  panel.background = element_rect(fill = NA, color = NA),
  panel.grid = element_blank(),
  panel.spacing.y = unit(0, "lines"),
  strip.text.y = element_blank(),
  legend.position = "bottom",
  legend.text = element_text(size = 9, color = "grey40"),
  legend.box.margin = margin(t = 30), 
  legend.background = element_rect(color = "grey40", 
                                   size = .3, 
                                   fill = "grey95"),
  legend.key.height = unit(.25, "lines"),
  legend.key.width = unit(2.5, "lines"),
  plot.margin = margin(c(20, 20, 20, 20)),
  axis.line.y = element_line(color="grey40", size = 0.4),
  axis.ticks.y = element_line(colour = "grey40", size = 0.4)
)
