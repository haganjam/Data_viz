
# data cleaning script

# load relevant data
library(readr)
library(dplyr)
library(ggplot2)

# next step is to use leaflet for Shiny
# https://rstudio.github.io/leaflet/shiny.html

# and to look at the flexdashboard themes
# https://rstudio.github.io/flexdashboard/articles/theme.html

# load the data
dat <- readr::read_csv("04-living-planet/data/LPD2022_public.csv")
head(dat)

# replace the null values with NAs
dat[dat == "NULL"] <- NA

# pipeline when a given id is selected
id <- unique(dat$ID)
x <- 
  dat |>
  dplyr::filter(ID == sample(id, 1)) |>
  tidyr::pivot_longer(cols = as.character(1950:2020),
                      names_to = "year",
                      values_to = "population_size") |>
  dplyr::mutate(year = as.numeric(year),
                population_size = as.numeric(population_size))

# remove the NA values in the population
x <- 
  x |>
  dplyr::filter(!is.na(population_size))

head(x)

ggplot(data = x,
       mapping = aes(x = year, y = log(1 + population_size) )) +
  geom_point() +
  geom_line() +
  scale_x_continuous(limits = c(1950, 2020))

