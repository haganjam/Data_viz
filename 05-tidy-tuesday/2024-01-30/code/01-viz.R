
# TidyTuesday: 2020-01-30

# load relevant libraries
library(ggplot2)
library(dplyr)
library(ggtext)
library(showtext)

# load the plotting theme
source("05-tidy-tuesday/2024-01-30/code/helper-plotting-theme.R")

# load the data directly from Github
groundhogs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-30/groundhogs.csv')
predictions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-30/predictions.csv')

# check the data
head(groundhogs)
head(predictions)

# how many states are there?
unique(groundhogs$region)

# how many are there from Canada and USA?
groundhogs |>
  dplyr::group_by(country) |>
  dplyr::summarise(n = n())

# just focus on the predictions data

# get the complete cases
predictions <- predictions[complete.cases(predictions$shadow), ]

# summarise the data into number of trials and number true
pred_sum <- 
  predictions |>
  dplyr::group_by(year) |>
  dplyr::summarise(n = n(),
                   success = sum(shadow), .groups = "drop") |>
  mutate(positive = success,
         negative = -((n-success)))

# check in which years, the proportions were different from random
pred_sum$sig_different <-
  sapply(1:nrow(pred_sum), function(x) {
    y <- prop.test(pred_sum[x,]$success, pred_sum[x,]$n, p = c(0.5))
    y$p.value < 0.20
    } )

# pull into the relevant long format
pred_long <- 
  pred_sum |>
  tidyr::pivot_longer(cols = c("positive", "negative"),
                      names_to = c("pos_neg"),
                      values_to = "votes")

# make sure that the significant effect is in the correct direction
pred_x <- split(pred_long, paste0(pred_long$year, pred_long$sig_different))
pred_x <- 
  lapply(pred_x, function(x) {
    y <- x$votes[1] > abs(x$votes[2])
    if(y == FALSE) {c(FALSE, TRUE)} else {c(TRUE, FALSE)}
} )
names(pred_x) <- NULL

# add this variable to the long format data
pred_long$sig_different_dir <- (pred_long$sig_different & do.call("c", pred_x))

# check the range of years
range(pred_long$year)

# check number of ground hogs
pred_sum

# save as a .rds file
saveRDS(pred_long, "06-banner-image/time-series.rds")

# set-up the legend
leg <- 
  dplyr::tibble(year = c(1887),
                y = c(30, 25, -30, -25),
                yend = c(32.5, 27.5, -32.5, -27.5),
                label = rep(c("- Consensus", "- Inconclusive"), 2),
                sig_different_dir = c(TRUE, FALSE, TRUE, FALSE),
                pos_neg = c("positive", "positive", "negative", "negative"),
                vjust = c(-0.6, -0.6, 1.4, 1.4))

# plot out the results
p1 <- 
  ggplot(data = pred_long) +
  geom_segment(
    data = leg,
    mapping = aes(x = year, xend = year, y = y, yend = yend, colour = pos_neg, alpha = sig_different_dir),
    inherit.aes = FALSE
  ) +
  geom_point(
    data = leg,
    mapping = aes(x = year, y = yend, colour = pos_neg, 
                  alpha = sig_different_dir, shape = sig_different_dir, size = sig_different_dir),
    inherit.aes = FALSE
  ) +
  geom_text(
    data = leg,
    mapping = aes(x = year, y = y, colour = pos_neg, label = label, vjust = vjust),
    size = 3.5,
    hjust = -0.2,
    family = "Ubuntu"
  ) +
  geom_segment(
    mapping = aes(x = 1969, xend = 1969, y = -42, yend = 41),
    linetype = "dotted",
    colour = "grey75"
  ) +
  geom_segment(
    mapping = aes(x = 1880, xend = 1880, y = -42, yend = 41),
    linewidth = 0.6,
    colour = "grey40"
  ) +
  geom_text(
    data = dplyr::tibble(year = c(1900, 1900),
                         votes = c(5, -5),
                         label = c("LONGER WINTER", 
                                   "EARLY SPRING   "),
                         pos_neg = c("positive", "negative")),
    mapping = aes(x = year, y = votes, label = label, colour = pos_neg),
    inherit.aes = FALSE, 
    size = 7.5, 
    family = "Bebas Neue",
    alpha = 0.6) +
  # ggtext::geom_richtext(
    # data = dplyr::tibble(year = c(1940, 1935),
                         # votes = c(5, -5),
                         # label = c("<span style='font-family:fa-solid'>&#xf2dc;</span>", 
                                   # "<span style='font-family:fa-solid'>&#xf06c;</span>"),
                         # pos_neg = c("positive", "negative")),
    # mapping = aes(x = year, y = votes, label = label, colour = pos_neg),
    # inherit.aes = FALSE, 
    # size = 7.5,
    # alpha = 0.9,
    # fill = NA, label.color = NA) +
  geom_segment(
    mapping = aes(x = 1880, xend = 2024, y = 0, yend = 0),
    linewidth = 0.35, colour = "grey40") +
  geom_segment(
    mapping = aes(x = year, xend = year, y = 0, yend = votes, colour = pos_neg, alpha = sig_different_dir)) +
  geom_point(
    mapping = aes(x = year, y = votes, colour = pos_neg, 
                  alpha = sig_different_dir, size = sig_different_dir,
                  shape = sig_different_dir)) +
  scale_colour_manual(
    values = c("#eba134","darkblue")
    ) +
  scale_alpha_manual(
    values = c(0.25, 1)
    ) +
  scale_size_manual(values = c(1, 3)) +
  scale_shape_manual(values = c(16, 18)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-50, 50), 
                     breaks = seq(-40, 40, 20), labels = abs, position = "left") +
  scale_x_continuous(limits = c(1880, 2100), 
                     breaks = round(seq(1887, 2023, length.out = 8), 0),
                     expand = c(0, 0)) +
  geom_segment(
    mapping = aes(x = 2023, xend = 2040, y = -42, yend = -32),
    colour = "#eba134"
  ) +
  xlab(NULL) +
  ylab("Number of predictions") +
  theme(legend.position = "none",
        axis.line.y = element_blank(),
        plot.title = element_blank(),
        axis.title.y = element_text(hjust = 0.5, colour = "grey40"),
        axis.text.x = element_text(vjust =10))
plot(p1)

# add text boxes
texts <-
  dplyr::tibble(
    year = c(2025, 1969),
    y = c(-30, 35),
    neg_pos = c("negative", "positive"),
    text = c(
      "In 2022, there was a consensus among groundhogs that it would be an early spring (41/79 votes),",
      "Prior to 1969, no more than four groundhog predictions were recorded in any given year."),
    vjust = c(0.15, 0),
    hjust = c(-0.2, 0.5)
  )

p2 <- 
  p1 +
  geom_textbox(
    data = texts,
    mapping = aes(
      x = year,
      y = y,
      label = text,
      colour = neg_pos,
      hjust = hjust,
      vjust = vjust
    ),
    inherit.aes = FALSE,
    size = 2.75,
    fill = "grey95",
    width = unit(120, "pt"),
    family = "Caslon")
plot(p2)

# description text
text_des <- dplyr::tibble(
  year = c(2067),
  y = c(18),
  text = c(
    "<b style='font-size:14pt'>**Groundhog Day weather predictions through time**</b><br><br> In North American tradition, groundhogs (*Marmota monax*) are consulted on the 2nd of February for a weather prediction. Specifically, if a groundhog is deemed to see its own shadow, there should be six more weeks of winter. If, however, the groundhog does not see its shadow, it means that there will be an early spring.<br><br>At **GROUNDHOG-DAY.COM**, they have collated these predictions from across Canada and the United States. Each vertical bar depicts the number of predictions made by groundhogs for either a longer winter or an early spring. Bars ending with a filled diamond indicate a consensus among groundhog's predictions. This consensus was determined by testing whether there was less than a 20% chance that the predictions were random using a test of equal proportions.<br><br>*Visualization by James G. Hagan*<br>Data from **GROUNDHOG-DAY.COM**"
    ))

p3 <-
  p2 +
  geom_textbox(
    data = text_des,
    mapping = aes(
      x = year,
      y = y,
      label = text
    ),
    inherit.aes = FALSE,
    size = 2.75,
    width = unit(200, "pt"),
    family = "Caslon",
    fill = NA,
    box.colour = NA,
    colour = "grey40") +
  theme(plot.margin = margin(c(10, 10, 10, 20)) )
plot(p3)

# add the tidytuesday hashtag
p4 <- 
  p3 +
  geom_textbox(
    mapping = aes(
      x = 2095,
      y = -42,
      label = "#TidyTuesday"
    ),
    inherit.aes = FALSE,
    size = 3.5,
    width = unit(200, "pt"),
    family = "Ubuntu",
    fill = NA,
    box.colour = NA,
    colour = "grey40",
    hjust = 0.3, 
    vjust = 1.5) +
  theme(plot.margin = margin(c(10, 10, -5, 20)))
plot(p4)

# export the figure
ggsave(filename = "05-tidy-tuesday/2024-01-30/figures-tables/fig1.pdf", p4,
       width = 10, height = 6, device = cairo_pdf)

pdftools::pdf_convert(pdf = "05-tidy-tuesday/2024-01-30/figures-tables/fig1.pdf",
                      filenames = "05-tidy-tuesday/2024-01-30/figures-tables/fig1.png",
                      format = "png", dpi = 400)


