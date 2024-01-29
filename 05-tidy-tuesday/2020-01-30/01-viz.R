
# TidyTuesday: 2020-01-30

# load relevant libraries
library(ggplot2)

# load the data directly from Github
groundhogs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-30/groundhogs.csv')
predictions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-30/predictions.csv')

# check the data
head(groundhogs)
head(predictions)

# just focus on the predictions data

# get the complete cases
predictions <- predictions[complete.cases(predictions$shadow), ]

# summarise the data into number of trials and number true
pred_sum <- 
  predictions |>
  dplyr::group_by(year) |>
  dplyr::summarise(n = n(),
                   success = sum(shadow), .groups = "drop") |>
  mutate(positive = success/n,
         negative = -((n-success)/n))

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
                      values_to = "proportion")

# plot out the results
ggplot(data = pred_long) +
  geom_segment(mapping = aes(x = year, xend = year, 
                             y = 0, yend = proportion, colour = pos_neg, alpha = sig_different)) +
  geom_point(mapping = aes(x = year, y = proportion, size = n, colour = pos_neg, alpha = sig_different)) +
  scale_colour_manual(values = c("darkblue", "gold")) +
  scale_alpha_manual(values = c(0.25, 1)) +
  scale_size_manual(values = c(0.5, 1))



