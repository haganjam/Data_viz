
# make the model gifs

# load relevant libraries
library(shiny)
library(shinyjs)
library(ggplot2)
library(gganimate)
library(ggtext)

# load the plotting theme
source("07-climate-models/helper-plotting-theme.R")

# make relevant plots for the app

# load the model data
ts_pub <- readRDS("07-climate-models/data/ts_pub.rds")
head(ts_pub)
summary(ts_pub)

# get the start and end dates of the models
ts_box <- 
  ts_pub |>
  dplyr::select(model_id, author, year_start, year_end) |>
  dplyr::distinct() |>
  dplyr::mutate(date_start = lubridate::make_date(year = year_start),
                date_end = lubridate::make_date(year = year_end)) |>
  dplyr::arrange(year_start) |>
  dplyr::mutate(label = paste0(author, " ", year_start),
                date_mid = date_start + floor((date_end-date_start)/2))

# load the observed data
ts_obs <- readRDS("07-climate-models/data/ts_obs.rds")
head(ts_obs)
summary(ts_obs)

# add a factor with different names for the sources
ts_obs$source_fac <- factor(ts_obs$source)
levels(ts_obs$source_fac) <- c("Berkeley Earth", "Cowtan and Way", "GISTEMP", "HadCRUT4", "NOAA")

# make a base observational data plot to highlight with the different boxes
base_obs_plot <-
  ggplot(data = ts_obs,
         mapping = aes(x = date, y = temp_anom_C, group = source_fac)) +
  geom_line(alpha = 0.2, linewidth = 0.1) +
  ylab("Temperature anomaly (\u00B0C)") +
  xlab(NULL) +
  scale_x_date() +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(-1, 1.4), clip = "off") +
  theme(plot.margin = margin(c(40, 10, 10, 10)))
plot(base_obs_plot)

# select a model
mod_id <- 
  ts_pub |>
  dplyr::arrange(year_start) |>
  dplyr::pull(model_id) |>
  unique()

# Manable 1970 was published first so I will reorder this manually
mod_id <- mod_id[c(2, 1, 3:length(mod_id))]

# create this kind of gif for all 10 models
mod_gif <- vector("list", length = length(mod_id))
for(j in 1:length(mod_gif)) {
  
  # add a box to this
  p_a1 <- 
    base_obs_plot +
    geom_segment(
      data = dplyr::filter(ts_box, model_id == mod_id[j]),
      mapping = aes(x = date_start, xend = date_start, y = -1, yend = 1.55),
      linetype = "dotted", colour = '#C71C7E',
      inherit.aes = FALSE
      ) +
    geom_text(
      data = dplyr::filter(ts_box, model_id == mod_id[j]),
      mapping = aes(x = date_start, y = 1.55, label = "Publication"),
      inherit.aes = FALSE,
      size = 3.5, 
      colour = '#C71C7E',
      family = "Dosis",
      vjust = -0.05
    ) +
    ggtext::geom_textbox(
      data = dplyr::filter(ts_box, model_id == mod_id[j]),
      mapping = aes(x = date_start, y = 1.3, label = "Prediction window"),
      inherit.aes = FALSE,
      size = 3, 
      alpha = 0.3,
      colour = 'grey45',
      family = "Bebas Neue",
      width = unit(60, "pt"),
      valign = 0.5,
      halign = 0.5,
      vjust = 1,
      hjust = 0.21,
      box.colour = NA,
      fill = NA
    ) +
    geom_rect(
      data = dplyr::filter(ts_box, model_id == mod_id[j]),
      mapping = aes(xmin = date_start, xmax = date_end, ymin = -1, ymax = 1.4),
      inherit.aes = FALSE,
      alpha = 0.1, 
      fill = '#C71C7E'
    ) +
    transition_layers(layer_length = 0.1, transition_length = 0.1, from_blank = FALSE) +
    theme(axis.title.y = element_text(vjust = 0.5,
                                      margin = margin(c(10, 30, 10, 10))),
          plot.margin = margin(c(35, 10, 20, 20)))
  # plot(p_a1)
  
  # write into a gif
  p_a1_gif <- animate(p_a1, width = 9, height = 7, renderer = gifski_renderer(), units = "cm", res = 150)
  p_a1_gifm <- magick::image_read(p_a1_gif)
  
  # make an animate plot
  pub_in <- 
    ts_pub |> 
    dplyr::filter(model_id == mod_id[j])
  
  obs_in <- 
    ts_obs |>
    dplyr::filter(date >= min(pub_in$date), date < max(pub_in$date))
  
  # get the min and max values
  min_y <- min( c(pub_in$temp_anom_C, obs_in$temp_anom_C) )
  max_y <- max( c(pub_in$temp_anom_C, obs_in$temp_anom_C) )
  
  p_a2 <- 
    ggplot() +
    geom_segment(
      data = dplyr::filter(ts_box, model_id == mod_id[j]),
      mapping = aes(x = date_start, xend = date_start, y = min_y-0.01, yend = max_y+0.01),
      linetype = "dotted", colour = '#C71C7E',
      inherit.aes = FALSE
    ) +
    geom_rect(
      data = dplyr::filter(ts_box, model_id == mod_id[j]),
      mapping = aes(xmin = date_start, xmax = date_end, ymin = min_y-0.01, ymax = max_y+0.01),
      inherit.aes = FALSE,
      alpha = 0.1, 
      fill = '#C71C7E'
    ) +
    geom_segment(
      data = dplyr::filter(ts_box, model_id == mod_id[j]),
      mapping = aes(x = lubridate::as_date(date_mid-1500), xend =  lubridate::as_date(date_mid-900),
                    y = max_y + 0.08, yend = max_y + 0.08),
      inherit.aes = FALSE
    ) +
    geom_text(
      data = dplyr::filter(ts_box, model_id == mod_id[j]),
      mapping = aes(x = lubridate::as_date(date_mid+300), y = max_y + 0.08,
                    label = "Model"),
      inherit.aes = FALSE,
      family = "Dosis"
    ) +
    geom_line(data = pub_in,
              mapping = aes(x = date, y = temp_anom_C)) +
    geom_line(data = obs_in,
              mapping = aes(x = date, y = temp_anom_C, group = source),
              alpha = 0.5, linewidth = 0.1) +
    ylab("Temperature anomaly (\u00B0C)") +
    xlab(NULL) +
    scale_x_date() +
    scale_y_continuous(expand = c(0, 0)) +
    coord_cartesian(ylim = c(min_y-0.01, max_y+0.085), clip = "off") +
    transition_reveal(date) +
    theme(axis.title.y = element_text(vjust = 0.5,
                                      margin = margin(c(10, 30, 10, 10))),
          plot.margin = margin(c(35, 10, 20, 20)))
  # plot(p_a2)
  
  # write into a gif
  p_a2_gif <- gganimate::animate(p_a2, width = 9, height = 7, renderer = gifski_renderer(), units = "cm", res = 150)
  p_a2_gifm <- magick::image_read(p_a2_gif)
  
  new_gif <- magick::image_montage(c(p_a1_gifm[length(p_a1_gifm)], p_a2_gifm[1]), tile = "2x1", geometry = "500x500", bg = "#f0e9df")
  for(i in 2:length(p_a2_gifm)) {
    combined <- magick::image_montage(c(p_a1_gifm[length(p_a1_gifm)], p_a2_gifm[i]), tile = "2x1", geometry = "500x500", bg = "#f0e9df")
    new_gif <- c(new_gif, combined)
  }
  
  # add to a list
  mod_gif[[j]] <- new_gif
  
}

# check the gifs
mod_gif[[2]]

# save the gifs
file_names <- as.character(c(paste0("_", 1:10)))
for(i in 1:length(mod_gif)) {
  
  magick::image_write_gif(
    mod_gif[[i]], paste0("07-climate-models/www/plot", file_names[i], ".gif")
    )
  
}

