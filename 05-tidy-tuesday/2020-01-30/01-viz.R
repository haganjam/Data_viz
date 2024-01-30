
# TidyTuesday: 2020-01-30

# load relevant libraries
library(ggplot2)
library(dplyr)
library(ggtext)
library(showtext)

# load the plotting theme
source("05-tidy-tuesday/2020-01-30/helper-plotting-theme.R")

# load some new fonts
font_add('fa-reg', 'fonts/Font Awesome 6 Free-Regular-400.otf')
font_add('fa-brands', 'fonts/Font Awesome 6 Brands-Regular-400.otf')
font_add('fa-solid', 'fonts/Font Awesome 6 Free-Solid-900.otf')

# add a gont from google fonts
font_add_google("Bebas Neue", "Bebas Neue")
font_add_google("Libre Caslon Text", "Caslon")

# activate showtext
showtext_auto()

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

# set-up the legend
leg <- 
  dplyr::tibble(year = c(1887),
                y = c(20, 15, -20, -15),
                yend = c(22.5, 17.5, -22.5, -17.5),
                label = rep(c("Consensus", "Inconclusive"), 2),
                sig_different_dir = c(TRUE, FALSE, TRUE, FALSE),
                pos_neg = c("positive", "positive", "negative", "negative"),
                vjust = c(-1, -1, 1, 1))

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
    size = 3,
    hjust = -0.2
  ) +
  geom_segment(
    mapping = aes(x = 1969, xend = 1969, y = -42, yend = 41),
    linetype = "dotted",
    colour = "grey75"
  ) +
  geom_segment(
    mapping = aes(x = 1880, xend = 1880, y = -42, yend = 41),
    linewidth = 0.225
  ) +
  geom_text(
    data = dplyr::tibble(year = c(1900, 1897),
                         votes = c(5, -5),
                         label = c("LONGER WINTER", 
                                   "EARLY SPRING"),
                         pos_neg = c("positive", "negative")),
    mapping = aes(x = year, y = votes, label = label, colour = pos_neg),
    inherit.aes = FALSE, 
    size = 7.5, 
    family = "Bebas Neue",
    alpha = 0.6) +
  ggtext::geom_richtext(
    data = dplyr::tibble(year = c(1940, 1935),
                         votes = c(5, -5),
                         label = c("<span style='font-family:fa-solid'>&#xf2dc;</span>", 
                                   "<span style='font-family:fa-solid'>&#xf06c;</span>"),
                         pos_neg = c("positive", "negative")),
    mapping = aes(x = year, y = votes, label = label, colour = pos_neg),
    inherit.aes = FALSE, 
    size = 7.5,
    alpha = 0.9,
    fill = NA, label.color = NA) +
  geom_segment(
    mapping = aes(x = 1880, xend = 2024, y = 0, yend = 0),
    linewidth = 0.225) +
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
  scale_y_continuous(expand = c(0, 0), limits = c(-43, 60), 
                     breaks = seq(-40, 40, 20), labels = abs, position = "left") +
  scale_x_continuous(limits = c(1880, 2060), 
                     breaks = round(seq(1887, 2023, length.out = 8), 0),
                     expand = c(0, 0)) +
  geom_segment(
    mapping = aes(x = 2023, xend = 2040, y = -42, yend = -32),
    colour = "#eba134"
  ) +
  xlab(NULL) +
  ylab("Number of votes") +
  theme(legend.position = "none",
        axis.line.y = element_blank())
plot(p1)

# add text
# add text boxes
texts <-
  tibble(
    year = c(2025, 1969),
    text = c(
      NA,
      "More than 90% of **Finland's** apartment blocks use CHP systems. Many are powered by renewable fuels, mostly wood.",
      NA,
      "**Germany** produces the most energy with CHP systems. Moreover, Germany's CHP systems are the most diversified and use all the different fuel sources.",
      NA,
      NA,
      "**Poland** uses solid fossil fuels (e.g. coal) for the majority of its CHP systems.",
      NA),
    vjust = 0.5
  )


