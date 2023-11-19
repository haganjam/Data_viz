#'
#' @title Who is the men's singles tennis GOAT?
#' 
#' @description Analysis of the greatest men's singles tennis player
#'

# load relevant libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggbeeswarm)
library(readr)
library(lubridate)

# load player id data
atp_players <- readr::read_csv(url("https://github.com/JeffSackmann/tennis_atp/raw/master/atp_players.csv"))
head(atp_players)

# load the atp rankings dataset
atp_rank <- readr::read_csv(url("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_rankings_current.csv"))
head(atp_rank)
dim(atp_rank)

# load the grand-slam winners dataset
gs_dat <- readr::read_csv("data/atp_tennis_data/atp_grand_slam_winners.csv")
head(gs_dat)

# federer first atp match: https://en.wikipedia.org/wiki/Roger_Federer#:~:text=1998–2002%3A%20Early%20professional%20career,-Main%20article%3A%20Roger&text=Federer%20made%20his%20ATP%20debut,in%20Toulouse%20against%20Guillaume%20Raoux.
fed_start <- 1998

# load the atp match datasets

# choose the years
years <- 2000:2023

# get the relevant linkes
links <- paste0("https://github.com/JeffSackmann/tennis_atp/raw/master/atp_matches_",
                as.character(years), ".csv")

# load the datasets into a list
atp_match_list <- vector("list", length = length(links))
for(i in 1:length(links)) {
  
  atp_match_list[[i]] <- readr::read_csv(url(links[i]))
  
}

# check the datasets
atp_match_list[[sample(x = 1:length(links), 1)]]

# get federer, nadal, djokovic and murray's stats
firstname <- c("Roger", "Rafael", "Novak", "Andy", "Stan")
surname <- c("Federer", "Nadal", "Djokovic", "Murray", "Wawrinka")

# get the player ids
ids <- 
  mapply(function(x, y) {
  
  atp_players |>
    dplyr::filter(name_first == x, name_last == y) |>
    dplyr::pull(player_id)
  
}, firstname, surname, SIMPLIFY = TRUE)
 
#'
#' @title win_percentage()
#' 
#' @description calculate a player's point winning percentage in each of their
#' matches in a given calendar year
#' 
#' @param data - atp match dataset (e.g. atp_matches_2000.csv) from https://github.com/JeffSackmann/tennis_atp/tree/master
#' @param player_id - id code for a player (e.g. 103819)

win_percentage <- function(data, player_id) {
  
  # when the player wins
  win_df <- 
    data |>
    dplyr::filter(winner_id == player_id)
  
  # calculate point winning percentage
  point_win1 <- 
    with(win_df, (w_1stWon + w_2ndWon + (l_svpt - l_1stWon - l_2ndWon))/(w_svpt + l_svpt) )
  
  # add this to the tournament date
  point_win1 <- 
    dplyr::tibble(player_id = win_df$winner_id,
                  player_name = win_df$winner_name,
                  tourney_year = substr(win_df$tourney_date, start = 1, stop = 4),
                  tourney_date = lubridate::ymd(win_df$tourney_date),
                  point_win_perc = point_win1,
                  rank = win_df$winner_rank)
  
  # when the player loses
  lose_df <- 
    atp_match2000 |>
    dplyr::filter(loser_id == player_id)
  
  # calculate point winning percentage
  point_win2 <- 
    with(lose_df, (l_1stWon + l_2ndWon + (w_svpt - w_1stWon - w_2ndWon))/(w_svpt + l_svpt) )
  
  # add this to the tournament date
  point_win2 <- 
    dplyr::tibble(player_id = lose_df$loser_id,
                  player_name = lose_df$loser_name,
                  tourney_year = substr(lose_df$tourney_date, start = 1, stop = 4),
                  tourney_date = lubridate::ymd(lose_df$tourney_date),
                  point_win_perc = point_win2,
                  rank = lose_df$loser_rank)
  
  # bind these datasets
  point_win <- dplyr::bind_rows(point_win1, point_win2)
  
  # arrange by date
  point_win <- 
    point_win |>
    dplyr::arrange(tourney_date) |>
    dplyr::mutate(tourney_year = as.numeric(tourney_year))
  
  point_win
  
}

# for each calendar year and each player, calculate the win percentage

# set-up an output list
win_perc_list <- vector("list", length = length(atp_match_list))

# test the function
win_percentage(data = atp_match_list[[1]], player_id = ids[[5]])




