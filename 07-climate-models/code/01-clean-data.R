
# clean the climate model data: 
# https://agupubs.onlinelibrary.wiley.com/doi/full/10.1029/2019GL085378

# load relevant libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

# load the data
ts_pub <- readr::read_csv("07-climate-models/data/model-timeseries-pubs.csv")
head(ts_pub)

# only get the temperature projects
ts_pub <- ts_pub[, grepl("_T", x = names(ts_pub)) | (names(ts_pub) == "Year") ]

# check the names
names(ts_pub)

# remove the C02 projection and forcing projection from Schneider_Thompson
ts_pub <- 
  ts_pub |>
  dplyr::select(-Schneider_Thompson_1981_CO2, -Schneider_Thompson_1981_F)

# keep rows with Year not NA
ts_pub <- 
  ts_pub |>
  dplyr::filter(!is.na(Year))
dim(ts_pub)

# pull into the long-format
ts_pub <- 
  ts_pub |>
  tidyr::pivot_longer(cols = names(ts_pub)[-1],
                      names_to = "model_id",
                      values_to = "temp_anom_C") |>
  dplyr::arrange(model_id, Year)

# check the unique model_ids
unique(ts_pub$model_id)

# keep the models with reasonable radiative forcings
rf <- c("Manabe_1970_T", "Mitchell_1970_T", "Benson_1970_T", "Sawyer_1972_T",
        "Broecker_1975_T", "Nordhaus_1977_T", "Schneider_Thompson_1981_T",
        "Hansen_1981_2a_T", "SAR_EBM_Best_T", "AR4_MMM_Best_T")

ts_pub <- 
  ts_pub |>
  dplyr::filter(model_id %in% rf)

# remove the NAs
ts_pub <- ts_pub[complete.cases(ts_pub),]

# make a dataset about what periods the predictions were made
# Fig. 1: https://agupubs.onlinelibrary.wiley.com/doi/full/10.1029/2019GL085378
ts_meta <-
  dplyr::tibble(model_id = unique(ts_pub$model_id),
                author = c("IPCC", "Benson", "Broecker", "Hansen", "Manabe", "Mitchell", "Nordhaus", "IPCC", "Sawyer", "Schneider & Thompson"),
                year_start = c(2007, 1970, 1975, 1981, 1970, 1970, 1977, 1995, 1972, 1981),
                year_end = c(2017, 2000, 2010, 2017, 2000, 2000, 2017, 2017, 2000, 2017))

# join these data to the ts_pub data
ts_pub <- dplyr::full_join(ts_pub, ts_meta, by = "model_id")

# reorganise the columns
ts_pub <-
  ts_pub |>
  dplyr::filter(Year >= year_start, Year <= year_end) |>
  dplyr::select(model_id, author, year_start, year_end, year = Year, temp_anom_C)

# convert the year variable into a data variable
ts_pub <- 
  ts_pub |>
  dplyr::mutate(date = lubridate::make_date(year = year, month = 6)) |>
  dplyr::select(model_id, author, year_start, year_end, year, date, temp_anom_C)

# check the summary of the dataset
summary(ts_pub)

# test plot
ggplot(data = ts_pub,
       mapping = aes(x = date, y = temp_anom_C, colour = model_id)) +
  geom_line()

saveRDS(object = ts_pub, file = "07-climate-models/data/ts_pub.rds")

# clean the observation data
ts_obs <- readr::read_csv("07-climate-models/data/obs-temperature.csv", col_select = -1)
head(ts_obs)

# create a proper year-month date column
# remove the copernicus column because it doesn't have good temporal resolution
ts_obs <- 
  ts_obs |>
  dplyr::mutate(date = lubridate::make_date(year = year, month = month)) |>
  dplyr::select(-copernicus)

# convert to the long format
ts_obs <- 
  ts_obs |>
  tidyr::pivot_longer(cols = c("hadcrut4", "gistemp", "noaa", "berkeley", "cowtan_way"),
                      names_to = "source",
                      values_to = "temp_anom_C")

# remove the NA values
ts_obs <- ts_obs[complete.cases(ts_obs),]

saveRDS(object = ts_obs, file = "07-climate-models/data/ts_obs.rds")

# summarise the observations
ts_obs_sum <- 
  ts_obs |>
  dplyr::group_by(year, month, date) |>
  dplyr::summarise(temp_anom_C_min = min(temp_anom_C),
                   temp_anom_C_max = max(temp_anom_C))

saveRDS(object = ts_obs_sum, file = "07-climate-models/data/ts_obs_sum.rds")

# test plot
ggplot(data = ts_obs_sum,
       mapping = aes(x = date)) +
  geom_ribbon(mapping = aes(ymin = temp_anom_C_min, 
                            ymax = temp_anom_C_max),
              alpha = 0.25)

summary(ts_obs)

ggplot(data = ts_obs,
       mapping = aes(x = date, y = temp_anom_C, colour = source)) +
  geom_line(alpha = 0.2) +
  theme(legend.position = "none")

### END
