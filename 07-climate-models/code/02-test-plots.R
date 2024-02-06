
# viz tests

# load the relevant libraries
library(ggplot2)
library(gganimate)
library(ggtext)

# load the plotting theme
source("07-climate-models/code/helper-plotting-theme.R")

# read the published models
ts_pub <- readRDS("07-climate-models/data/ts_pub.rds")
head(ts_pub)

# read the summarised observations
ts_obs <- readRDS("07-climate-models/data/ts_obs.rds")
head(ts_obs)

# make an animate plot
pub_in <- 
  ts_pub |> 
  dplyr::filter(model_id == "AR4_MMM_Best_T")
min(pub_in$date)

obs_in <- 
  ts_obs |>
  dplyr::filter(date >= min(pub_in$date), date < max(pub_in$date))

ggplot() +
  geom_line(data = pub_in,
            mapping = aes(x = date, y = temp_anom_C)) +
  geom_line(data = obs_in,
             mapping = aes(x = date, y = temp_anom_C, group = source),
            alpha = 0.5, linewidth = 0.1) +
  transition_reveal(date)




