#'
#' @title Do tennis players have an optimal height?
#' 
#' @description I have the notion that most of the best tennis players i.e. those
#' that have won more than one grand slams fall into a relatively narrow height range. 
#'

# load relevant libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(lubridate)

# load the relevant plotting theme
source("01-atp-GOAT/code/helper-plotting-theme.R")

# load player id data
atp_players <- readr::read_csv(url("https://github.com/JeffSackmann/tennis_atp/raw/master/atp_players.csv"))
head(atp_players)
dim(atp_players)

# load the atp rankings dataset
# load the atp match datasets

# choose the years
decades <- c("80s", "90s", "00s", "10s", "20s", "current")

# get the relevant linkes
links <- paste0("https://github.com/JeffSackmann/tennis_atp/raw/master/atp_rankings_",
                as.character(decades), ".csv")

# load the datasets into a list
atp_rank_list <- vector("list", length = length(links))
for(i in 1:length(links)) {
  
  # read the data
  x <- readr::read_csv(url(links[i]))
  
  # convert the date to a proper date variable
  x <- 
    x |>
    dplyr::mutate(ranking_date = paste0(substr(ranking_date, 1, 4), "-", 
                                        substr(ranking_date, 5, 6), "-",
                                        substr(ranking_date, 7, 8))) |>
    dplyr::mutate(ranking_date = ymd(ranking_date))
  
  # add it to a list
  atp_rank_list[[i]] <- x
  
}

# check the datasets
atp_rank_list[[sample(x = 1:length(links), 1)]]

# bind into a big dataset
atp_rank <- dplyr::bind_rows(atp_rank_list)

# get the players with a ranking post 1985
play_id <- 
  atp_rank |>
  dplyr::filter(ranking_date >= ymd(c("1985-01-01"))) |>
  dplyr::pull(player) |>
  unique()

# check how many players were ranked between 1985 and at present
length(play_id)

# load the grand-slam winners dataset
gs_dat <- readr::read_csv("01-atp-GOAT/data/atp_grand_slam_winners.csv")
head(gs_dat)

# extract the names of the grandslam winners since 1985
gs_win <- 
  gs_dat |>
  dplyr::filter(YEAR %in% c(1985:2023)) |>
  dplyr::group_by(WINNER) |>
  dplyr::summarise(n = n()) |>
  dplyr::filter(n > 1) |>
  dplyr::pull(WINNER) |>
  unique()

# get the player ids
gs_id <- 
  sapply(gs_win, function(x) {
    
    y <- strsplit(x = x, " ")
    
    atp_players |>
      dplyr::filter(name_first == y[[1]][1], name_last == y[[1]][2]) |>
      dplyr::pull(player_id)
    
  })

# get the players that were on the tour between 1985 and present
gs_h <- 
  atp_players |>
  dplyr::filter(player_id %in% c(play_id, gs_id)) |>
  dplyr::mutate(gs_winner = ifelse(player_id %in% gs_id, TRUE, FALSE))
dim(gs_h)

# check this data
gs_h |>
  dplyr::filter(gs_winner == TRUE)

# remove any players without height data
gs_h <- gs_h[complete.cases(gs_h$height),]
dim(gs_h)

# make a first plot
ggplot() +
  geom_density(data = gs_h,
                mapping = aes(x = height), adjust = 2, alpha = 0.2, fill = "darkblue") +
  geom_vline(data = gs_h |> dplyr::filter(gs_winner == TRUE),
             mapping = aes(xintercept = height))

# make a bunch of random distributions

# how many double grand slams winners are there?
gs_n <- length(gs_id)

# how many samples?
N <- 100

# make a list to receive the output
ran_list <- vector("list", length = N)
for(i in 1:N) {
  
  x <- 
    gs_h |> 
    dplyr::filter(gs_winner == FALSE)
  
  ran_list[[i]] <- x[sample(x = 1:nrow(x), size = gs_n), ]
  
}

# combine into a data.frame
ran_df <- dplyr::bind_rows(ran_list, .id = "sample")

ggplot() +
  geom_density(data = ran_df,
               mapping = aes(x = height, group = sample), adjust = 2, alpha = 0.05, 
               fill = "darkblue", colour = NA) +
  geom_density(data = gs_h |> dplyr::filter(gs_winner == TRUE),
               mapping = aes(x = height), adjust = 2, alpha = 0.5, fill = "gold", colour = NA)

# summarise the random data.frame
ran_df_sum <- 
  ran_df |>
  dplyr::group_by(sample) |>
  dplyr::summarise(h_m = mean(height),
                   h_sd = sd(height))

# summarise the grandslam data
gs_sum <- 
  gs_h |> 
  dplyr::filter(gs_winner == TRUE) |>
  dplyr::summarise(h_m = mean(height),
                   h_sd = sd(height))

ggplot(data = ran_df_sum,
       mapping = aes(x = sample)) +
  geom_errorbar(mapping = aes(ymin = h_m - h_sd, ymax = h_m + h_sd),
                width = 0, alpha = 0.3) +
  geom_errorbar(data = gs_sum,
                mapping = aes(x = 50, ymin = h_m - h_sd, ymax = h_m + h_sd),
                colour = "red") +
  coord_flip()















