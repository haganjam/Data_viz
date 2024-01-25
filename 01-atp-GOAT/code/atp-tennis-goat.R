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
library(gam)

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
start <- 1998

# load the atp match datasets

# choose the years
years <- start:2023

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

# get get players that have won more than one slam since 1995
which_players <- c("Pete Sampras", "Andre Agassi", "Gustavo Kuerten",
                   "Marat Safin", "Lleyton Hewitt", "Roger Federer", 
                   "Rafael Nadal", "Novak Djokovic", "Andy Murray",
                   "Stan Wawrinka", "Carlos Alcaraz")

# get the player ids
ids <- 
  sapply(which_players, function(x) {
  
    y <- strsplit(x = x, " ")
    
    atp_players |>
      dplyr::filter(name_first == y[[1]][1], name_last == y[[1]][2]) |>
      dplyr::pull(player_id)
  
})
print(ids)
 
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
    data |>
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

for(i in 1:length(win_perc_list)) {
  
  # calculate win percentage for each player in year i
  y <- 
    
    lapply(ids, function(x) {
    
    win_percentage(data = atp_match_list[[i]], player_id = x)
    
  })
  
  # bind into a data.frame
  win_perc_list[[i]] <- dplyr::bind_rows(y)
  
}

# bind this into one large dataset
win_perc_df <- dplyr::bind_rows(win_perc_list, .id = "year")
dim(win_perc_df)

# remove the NA values
win_perc_df <- 
  win_perc_df |>
  dplyr::filter(!is.na(point_win_perc))

# make a variable for whether it is federer, nadal, djokovic or not
win_perc_df <- 
  win_perc_df |>
  mutate(goats = ifelse(player_id %in% c(103819, 104745, 104925), TRUE, FALSE))

# check how many tournaments each player per year
win_perc_df |>
  dplyr::group_by(player_id, player_name, tourney_year) |>
  dplyr::summarise(n = length(unique(tourney_date))) |>
  View()

# exclude years where less than 5 tournaments were played summarise by tourney date
win_perc_sum <- 
  win_perc_df |>
  dplyr::group_by(player_id, player_name, tourney_year) |>
  dplyr::mutate(n = length(unique(tourney_date))) |>
  dplyr::filter(n > 4) |>
  dplyr::ungroup() |>
  dplyr::group_by(goats, player_id, player_name, tourney_date) |>
  dplyr::summarise(point_win_perc_m = mean(point_win_perc),
                   point_win_perc_sd = sd(point_win_perc)
                   )

# fit gams to these data

# get confidence intervals: https://fromthebottomoftheheap.net/2016/12/15/simultaneous-interval-revisited/
gam_df <- 
  win_perc_sum |>
  dplyr::filter(player_id == 103819) |>
  dplyr::mutate(tourney_date_num = as.numeric(tourney_date))
  
mod_gam1 <- gam(point_win_perc_m ~ s(as.numeric(tourney_date_num), 3), data = gam_df)
plot(mod_gam1)

pred_df <- predict(mod_gam1, type = "link", se.fit = TRUE)

# plot the results


# plot point win percentage over time
ggplot() +
  geom_smooth(data = win_perc_sum |> dplyr::filter(goats == FALSE),
             mapping = aes(x = tourney_date, y = point_win_perc_m, group = player_name),
             show.legend = FALSE, alpha = 0.1, linewidth = 0.2, colour = "grey") +
  # geom_point(data = win_perc_sum |> dplyr::filter(goats == TRUE),
             # mapping = aes(x = tourney_date, y = point_win_perc_m, colour = player_name),
             # alpha = 0.1) +
  geom_smooth(data = win_perc_sum |> dplyr::filter(goats == TRUE),
             mapping = aes(x = tourney_date, y = point_win_perc_m, colour = player_name)) +
  theme_classic() +
  theme(legend.position = "bottom")




