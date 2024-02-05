
# tidytuesday: 2024-02-06

# load relevant libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggtext)

# load the plotting theme
source("05-tidy-tuesday/2024-02-06/code/helper-plotting-theme.R")

# load the data
her <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-06/heritage.csv')
head(her)
dim(her)

# convert the data into the tidy format
her <- 
  her |>
  tidyr::pivot_longer(cols = c(`2004`, `2022`),
                      names_to = "year",
                      values_to = "sites") |>
  dplyr::mutate(year = as.numeric(year))

# replicate each row with the same number of sites
her_list <- split(her, paste0(her$country, her$year))

her_list <- 
  
  lapply(her_list, function(x) {
  
  y <- x
  for(i in 2:x$sites) {
    y <- dplyr::bind_rows(y, x)
  }
  y
  
} )
names(her_list) <- 1:length(her_list)
her_df <- dplyr::bind_rows(her_list, .id = "group")

# make all the sites the same
her_df <- 
  her_df |>
  dplyr::mutate(sites = ifelse(country == "Norway", 2.25, 
                               ifelse(country == "Denmark", 0.75, 
                                      ifelse(country == "Sweden", 1.5, NA))))

# make a colour column for the new sites
her_df$new_yn <- "no"

# check the data
her

# get rid of the relevant yes values
cond <- (her_df$country == "Denmark") & (her_df$year == 2022)
l <- sum(cond)
her_df[ cond, ][["new_yn"]][(l-6+1):l] <- "yes"

cond <- (her_df$country == "Norway") & (her_df$year == 2022)
l <- sum(cond)
her_df[ cond, ][["new_yn"]][(l-3+1):l] <- "yes"

cond <- (her_df$country == "Sweden") & (her_df$year == 2022)
l <- sum(cond)
her_df[ cond, ][["new_yn"]][(l-2+1):l] <- "yes"

# check the number of replicates for each group
min_sites <- 
  her_df |>
  dplyr::group_by(country, year) |>
  dplyr::summarise(n = n()) |>
  dplyr::filter(year == 2004) |>
  dplyr::pull(n)

# set-up the jitter variables needed
jit_list <- vector("list", length = length(min_sites))
for(i in 1:length(min_sites)) {
  jit_year <- runif(n = min_sites[i], -1, 1)
  jit_sites <- runif(n = min_sites[i], -0.2, 0.2)
  jit_list[[i]] <- list(jit_year, jit_sites)
}
names(jit_list) <- c("Denmark", "Norway", "Sweden")

# make a jittered year variable
her_df$year_var <- her_df$year + c(jit_list$Denmark[[1]], c(jit_list$Denmark[[1]], runif(n = 6, -1, 1)),
                                   jit_list$Norway[[1]], c(jit_list$Norway[[1]], runif(n = 3, -1, 1)),
                                   jit_list$Sweden[[1]], c(jit_list$Sweden[[1]], runif(n = 2, -1, 1)))

# make a jittered site variable
her_df$site_var <- her_df$sites + c(jit_list$Denmark[[2]], c(jit_list$Denmark[[2]], runif(n = 6, -0.2, 0.2)),
                                   jit_list$Norway[[2]], c(jit_list$Norway[[2]], runif(n = 3, -0.2, 0.2)),
                                   jit_list$Sweden[[2]], c(jit_list$Sweden[[2]], runif(n = 2, -0.2, 0.2)))

# colour palette for the points
cols <- c("#ECF8F8", "#C65B94")

p1 <- 
  ggplot(
  data = her_df,
  mapping = aes(x = year, y = site_var, colour = new_yn) 
  ) +
  geom_segment(
    data = dplyr::tibble(year_min = rep(2000, 3),
                         year_max = rep(2023, 3),
                         y = c(0.75, 1.5, 2.25)),
    mapping = aes(x = year_min, xend = year_max, y = y, yend = y),
    inherit.aes = FALSE,
    colour = "grey45",
    alpha = 0.25
  ) +
  geom_text(
    data = dplyr::tibble(year = c(rep(2004, 3), rep(2022, 3)),
                         sites = rep(c(0.75, 1.5, 2.25), 2),
                         label = c("4 ", 
                                   "13",
                                   "5 ",
                                   "10",
                                   "15",
                                   "8 ")),
    mapping = aes(x = year, y = sites, label = label),
    inherit.aes = FALSE, 
    size = 12, 
    colour = "#8E8A8C",
    family = "Bebas Neue",
    alpha = 0.75,
    hjust = 1.35) +
  geom_text(
    data = dplyr::tibble(year = rep(2012, 3),
                         sites = c(0.75, 1.5, 2.25),
                         label = c("+6", 
                                   "+2",
                                   "+3")),
    mapping = aes(x = year, y = sites, label = label),
    inherit.aes = FALSE, 
    size = 10, 
    colour = "#C65B94",
    family = "Bebas Neue",
    alpha = 0.75) +
  ggbeeswarm::geom_quasirandom(size = 2.5, width = 0.5, alpha = 1, shape = 1, stroke = 0.75) +
  # geom_point(size = 2.5) +
  scale_colour_manual(values = cols) +
  scale_x_continuous(
    limits = c(1998, 2023), 
    breaks = c(2004, 2022)
    ) +
  scale_y_continuous(limits = c(0.2, 2.9)) +
  ggtext::geom_richtext(
    data = dplyr::tibble(year = rep(2002.5, 3),
                         sites = c(0.77, 1.52, 2.27),
                         label = c("<span style='color:#C8102E;'>D<span style='color:#FFFFFF;'>N<span style='color:#C8102E;'>K", 
                                   "<span style='color:#006AA7;'>S<span style='color:#FECC02;'>W<span style='color:#006AA7;'>E", 
                                   "<span style='color:#BA0C2F;'>N<span style='color:#FFFFFF;'>O<span style='color:#00205B;'>R")),
    mapping = aes(x = year, y = sites, label = label),
    inherit.aes = FALSE, 
    size = 10, 
    fill = NA,
    colour = NA,
    family = "Bebas Neue",
    alpha = 0.6,
    hjust = 2) +
  ylab(NULL) +
  xlab(NULL) +
  ggtext::geom_textbox(
    data = dplyr::tibble(year = c(2010.2),
                         sites = c(2.85),
                         label = c("In 2004, the three Scandinavian countries (Denmark - DNK, Sweden - SWE and Norway - NOR) had 22 UNESCO world heritage sites between them. In 18 years, this rose to 33 with the number in Denmark going from 4 to 10, a more than 100% increase.")),
    mapping = aes(x = year, y = sites, label = label),
    inherit.aes = FALSE, 
    size = 3.5, 
    colour = "black",
    box.colour = NA,
    fill = NA,
    width = unit(520, "pt"),
    alpha = 0.75,
    hjust = 0.495) +
  ggtext::geom_textbox(
    data = dplyr::tibble(year = c(2002),
                         sites = c(0.4),
                         label = c("*Visualization by James G. Hagan*<br>Data from **100.datavizproject.com**")),
    mapping = aes(x = year, y = sites, label = label),
    inherit.aes = FALSE, 
    size = 3, 
    colour = "black",
    box.colour = NA,
    fill = NA,
    width = unit(200, "pt"),
    alpha = 0.75,
    hjust = 0.495,
    vjust = 1.75) +
  ggtext::geom_textbox(
    data = dplyr::tibble(year = c(2023),
                         sites = c(0.3),
                         label = c("**#TidyTuesday**")),
    mapping = aes(x = year, y = sites, label = label),
    inherit.aes = FALSE, 
    size = 3.5, 
    colour = "black",
    box.colour = NA,
    fill = NA,
    width = unit(200, "pt"),
    alpha = 0.75,
    vjust = 1.85,
    hjust = 0.35) +
  ggtitle("UNESCO World Heritage Sites in Scandinavia (2004-2022)") +
  theme(legend.position = "none") +
  theme(plot.title = element_text(size = 22),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_text(size = 14, vjust = 19, hjust = 0.9))
plot(p1)

# export the figure
ggsave(filename = "05-tidy-tuesday/2024-02-06/figures-tables/fig1.pdf", p1,
       width = 2400, height = 1900, units = "px", device = cairo_pdf)

pdftools::pdf_convert(pdf = "05-tidy-tuesday/2024-02-06/figures-tables/fig1.pdf",
                      filenames = "05-tidy-tuesday/2024-02-06/figures-tables/fig1.png",
                      format = "png", dpi = 400)



