library(dplyr)

messy_stats_1st <- messy_stats_1st %>%
    mutate(play_key = paste(game_str, play_id, sep = "_"))


# Getting how close the runner is to home at every point in play
go_home_1st <- player_pos %>%
    # Create composite key
    mutate(play_key = paste(game_str, play_id, sep = "_")) %>%
    filter(
        play_key %in% messy_stats_1st$play_key,
        player_position == 11
    ) %>%
    # Deselect some unnecessary columns
    dplyr::select(-c("Season", "HomeTeam", "AwayTeam", "Day")) %>%
    collect()

go_home_1st <- go_home_1st %>%
    mutate(home_dist = sqrt(field_x^2 + field_y^2) )


# Seeing if the player went home based on their closest distance to home during the play
go_home_1st <- go_home_1st %>%
    mutate(going = ifelse( home_dist <= 45, 1, 0) )

close_home_1st <- go_home_1st %>%
    group_by(play_key) %>%
    slice_min(order_by = home_dist, n = 1, with_ties = FALSE) %>%
    ungroup()


# Getting the timestamp at which they are closest to home
close_home_1st <- close_home_1st %>% rename(run_home_time = timestamp)

messy_stats_1st <- full_join(messy_stats_1st, close_home_1st[,c(3,7:9)], by = "play_key")


# Seeing if and when the catcher acquired the ball during the play
catcher_rec_1st <- game_events %>%
    # Create composite key
    mutate(play_key = paste(game_str, play_id, sep = "_")) %>%
    filter(
        play_key %in% messy_stats_1st$play_key ,
        player_position == 2 ,
        event_code == 2
    ) %>%
    # Deselect some unnecessary columns
    dplyr::select(-c("Season", "HomeTeam", "AwayTeam", "Day", "at_bat", "play_per_game")) %>%
    collect()

catcher_rec_1st <- catcher_rec_1st %>% rename(catch_time = timestamp)

messy_stats_1st <- full_join(messy_stats_1st, catcher_rec_1st[,c(3,6)], by = "play_key")


# Comparing time the catcher acquired the ball and time the runner was closest to home
messy_stats_1st <- messy_stats_1st %>%
    mutate(run_min_catch_time = (run_home_time - catch_time) / 1000)

messy_stats_1st <- messy_stats_1st %>%
    mutate(catch_before_score = ifelse( run_min_catch_time > 0, 1, 0 ) )


# Organzing needed data into a new dataframe
stats_1st <- messy_stats_1st %>%
    dplyr::select(game_str, play_id, run_dist, OF_dist, run_speed, OF_momentum_home, OF_momentum_side, home_dist, going, run_min_catch_time, catch_before_score)


# Used the runner and catcher info here to determine whether there is a chance the runner was out, and animating those plays to determine the chance they were out, did this info in Excel

final_1st_stats <- read_csv("final_1st_stats.csv")

# Filtering out plays I saw that made them non applicable
final_1st_stats <- final_1st_stats %>%
    filter(!is.na(score_chance))

final_1st_stats <- final_1st_stats %>%
    mutate( safe_out = ifelse(score_chance == -1, NA, ifelse(score_chance <= 0.5, 0, 1) ) )
