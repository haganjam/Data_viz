
# plotting theme from:
# https://github.com/z3tt/TidyTuesday/blob/main/R/2020_27_ClaremontRunXMen.Rmd

## ggplot theme
theme_set(theme_minimal(base_family = "Courier New", base_size = 12))

theme_update(
  plot.title = element_text(size = 27,
                            face = "bold",
                            hjust = .5,
                            margin = margin(5, 0, 5, 0)),
  plot.caption = element_text(size = 9,
                              color = "grey40",
                              hjust = .5,
                              margin = margin(20, 0, 5, 0)),
  axis.text.y = element_blank(),
  axis.title = element_blank(),
  plot.background = element_rect(fill = "#FDEBD0", color = NA),
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
  plot.margin = margin(c(20, 20, 20, 20))
)
