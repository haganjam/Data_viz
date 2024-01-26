
# EU energy generation through time

# load relevant libraries
library(readxl)
library(dplyr)

# replace the eu_member names
eu_member <- c(
  "Belgium",
  "Bulgaria",
  "Czech Republic",
  "Denmark",
  "Germany",
  "Estonia",
  "Ireland",
  "Greece",
  "Spain",
  "France",
  "Croatia",
  "Italy",
  "Cyprus",
  "Latvia",
  "Lithuania",
  "Luxembourg",
  "Hungary",
  "Malta",
  "Netherlands",
  "Austria",
  "Poland",
  "Portugal",
  "Romania",
  "Slovenia",
  "Slovakia",
  "Finland",
  "Sweden",
  "Turkey",
  "Norway",
  "United Kingdom"
)

# set-up the years
years <- 2019:2005

# loop over each sheet
ener_dat <- vector("list", length = length(years))
for(i in 1:length(years)) {
  
  # load the data
  ener <- readxl::read_excel("02-EU-energy/data/CHPdata2005-2019.xlsx", 
                             sheet = 1+i, skip = 3)
  
  # keep relevant rows
  ener <- ener[(which(ener[[1]] == years[i])[1]):nrow(ener), ]
  ener[[1]] <- gsub(pattern = "[0-9]", replacement = "", ener[[1]])
  ener <- ener[ener[[1]] %in% eu_member, ]
  
  # remove all columns with only NA values
  x <- apply(ener, 2, function(x) all(is.na(x)))
  names(x) <- NULL
  ener <- ener[, !x]
  
  # keep relevant columns
  ener <- ener[, c(1, 2, rev(ncol(ener):(ncol(ener)-4)))]

  # rename the columns
  names(ener) <- c("eu_member", "total", "solid_ff", "oil", "gas", "renew", "other")
  
  # add a column for the year
  ener$year <- years[i]
  
  # get relevant columns
  ener <- 
    ener |>
    dplyr::select(eu_member, year, total, solid_ff, oil, gas, renew, other)
  
  ener_dat[[i]] <- ener
  
}

# bind into a data.frame
ener_dat <- dplyr::bind_rows(ener_dat)

# arrange the data
ener_dat <- dplyr::arrange(ener_dat, eu_member, year)

# check the structure of the variables
str(ener_dat)

# round the numeric variables
ener_dat <- 
  ener_dat |>
  mutate(across(.cols = c("total", "solid_ff", "oil", "gas", "renew", "other"), ~as.numeric(.))) |>
  mutate(across(.cols = c("total", "solid_ff", "oil", "gas", "renew", "other"), ~round(., 3)))

# check that the percentages add to 100
x <- apply(ener_dat[,-c(1:3)], 1, function(x) sum(x))
range(x, na.rm = TRUE )
ener_dat[which(x == 0),]

# remove the rows where malta is zero
ener_dat <- ener_dat[-which(x == 0) ,]

# get the complete cases
ener_dat <- ener_dat[complete.cases(ener_dat),]

# export the cleaned data
saveRDS(ener_dat, "02-EU-energy/data/EU_energy_data.rds")

### END
