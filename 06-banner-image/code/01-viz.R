
# Make a banner image for my behance profile

# load relevant libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(showtext)
library(wesanderson)

# load the relevant plotting theme
source("06-banner-image/code/helper-plotting-theme.R")

# load the groundhog day data
dat <- readRDS("06-banner-image/time-series.rds")

# reverse the direction of the negative variable
dat$votes <- ifelse(dat$votes < 0, (-1*dat$votes), dat$votes)

# seperate the yellow and blue
dat$year <- ifelse(dat$pos_neg == "negative", dat$year + 0.5, dat$year)

# full long image
p1 <- 
  ggplot(data = dat) +
  geom_segment(
    mapping = aes(x = year, xend = year, y = 0, yend = votes, 
                  colour = pos_neg, alpha = sig_different_dir)) +
  geom_point(
    mapping = aes(x = year, y = votes,
                  alpha = sig_different_dir, size = sig_different_dir,
                  shape = sig_different_dir, colour = pos_neg)) +
  scale_size_manual(values = c(1, 2.5)) +
  scale_alpha_manual(
    values = c(0.5, 1)
  ) +
  scale_colour_manual(
    values = c("#eba134","darkblue")
  ) +
  scale_shape_manual(values = c(16, 18)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 50), 
                     breaks = seq(-40, 40, 20), labels = abs, position = "left") +
  scale_x_continuous(limits = c(1830, 2024), 
                     breaks = round(seq(1887, 2023, length.out = 8), 0),
                     expand = c(0, 0)) +
  geom_text(
    data = dplyr::tibble(year = c(1950),
                         votes = c(30),
                         label = c("JAMES G. HAGAN")),
    mapping = aes(x = year, y = votes, label = label),
    inherit.aes = FALSE, 
    size = 8, 
    family = "Bebas Neue",
    alpha = 0.6,
    colour = "grey25") +
  geom_text(
    data = dplyr::tibble(year = c(1950),
                         votes = c(20),
                         label = c("Data analysis and visualisation")),
    mapping = aes(x = year, y = votes, label = label),
    inherit.aes = FALSE, 
    size = 3.5, 
    family = "Ubuntu",
    alpha = 0.8,
    colour = "grey25") +
  theme(legend.position = "none",
        axis.line.y = element_blank(),
        plot.title = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = margin(c(20, 5, 0, 20)))
plot(p1)

ggsave(filename = "06-banner-image/figures-tables/fig1.pdf",
       p1, 
       width = 3200, height = 450, units = "px", device = cairo_pdf)

pdftools::pdf_convert(pdf = "06-banner-image/figures-tables/fig1.pdf",
                      filenames = "06-banner-image/figures-tables/fig1.png",
                      format = "png", dpi = 400)

# reduced image for twitter
p1 <- 
  ggplot(data = dat) +
  geom_segment(
    mapping = aes(x = year, xend = year, y = 0, yend = votes, 
                  colour = pos_neg, alpha = sig_different_dir)) +
  geom_point(
    mapping = aes(x = year, y = votes,
                  alpha = sig_different_dir, size = sig_different_dir,
                  shape = sig_different_dir, colour = pos_neg)) +
  scale_size_manual(values = c(1, 2.5)) +
  scale_alpha_manual(
    values = c(0.5, 1)
  ) +
  scale_colour_manual(
    values = c("#eba134","darkblue")
  ) +
  scale_shape_manual(values = c(16, 18)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 50), 
                     breaks = seq(-40, 40, 20), labels = abs, position = "left") +
  scale_x_continuous(limits = c(1890, 2024), 
                     breaks = round(seq(1887, 2023, length.out = 8), 0),
                     expand = c(0, 0)) +
  geom_text(
    data = dplyr::tibble(year = c(1970),
                         votes = c(35),
                         label = c("JAMES G. HAGAN")),
    mapping = aes(x = year, y = votes, label = label),
    inherit.aes = FALSE, 
    size = 8, 
    family = "Bebas Neue",
    alpha = 0.6,
    colour = "grey25") +
  geom_text(
    data = dplyr::tibble(year = c(1970),
                         votes = c(27.5),
                         label = c("Data analysis and visualisation")),
    mapping = aes(x = year, y = votes, label = label),
    inherit.aes = FALSE, 
    size = 3.5, 
    family = "Ubuntu",
    alpha = 0.8,
    colour = "grey25") +
  theme(legend.position = "none",
        axis.line.y = element_blank(),
        plot.title = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = margin(c(10, 5, 0, 0)))
plot(p1)

ggsave(filename = "06-banner-image/figures-tables/fig2.pdf",
       p1, 
       width = 1500, height = 500, units = "px")

pdftools::pdf_convert(pdf = "06-banner-image/figures-tables/fig2.pdf",
                      filenames = "06-banner-image/figures-tables/fig2.jpeg",
                      format = "jpeg", dpi = 3000)



