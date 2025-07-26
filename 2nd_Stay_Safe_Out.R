messy_stats <- messy_stats %>%
    mutate(play_key = paste(game_str, play_id, sep = "_"))


# Getting how close the runner is to home at every point in play
go_home <- player_pos %>%
    # Create composite key
    mutate(play_key = paste(game_str, play_id, sep = "_")) %>%
    filter(
        play_key %in% messy_stats$play_key,
        player_position == 12
    ) %>%
    # Deselect some unnecessary columns
    select(-c("Season", "HomeTeam", "AwayTeam", "Day")) %>%
    collect()

go_home <- go_home %>%
    mutate(home_dist = sqrt(field_x^2 + field_y^2) )


# Seeing if the player went home based on their closest distance to home during the play
go_home <- go_home %>%
    mutate(going = ifelse( home_dist <= 45, 1, 0) )

close_home <- go_home %>%
    group_by(play_key) %>%
    slice_min(order_by = home_dist, n = 1, with_ties = FALSE) %>%
    ungroup()


# Getting the timestamp at which they are closest to home
close_home <- close_home %>% rename(run_home_time = timestamp)

messy_stats <- full_join(messy_stats, close_home[,c(3,7:9)], by = "play_key")


# Seeing if and when the catcher acquired the ball during the play
catcher_rec <- game_events %>%
    # Create composite key
    mutate(play_key = paste(game_str, play_id, sep = "_")) %>%
    filter(
        play_key %in% messy_stats$play_key ,
        player_position == 2 ,
        event_code == 2
    ) %>%
    # Deselect some unnecessary columns
    select(-c("Season", "HomeTeam", "AwayTeam", "Day", "at_bat", "play_per_game")) %>%
    collect()

catcher_rec <- catcher_rec %>% rename(catch_time = timestamp)

messy_stats <- full_join(messy_stats, catcher_rec[,c(3,6)], by = "play_key")


# Comparing time the catcher acquired the ball and time the runner was closest to home
messy_stats <- messy_stats %>%
    mutate(run_min_catch_time = (run_home_time - catch_time) / 1000)

messy_stats <- messy_stats %>%
    mutate(catch_before_score = ifelse( run_min_catch_time > 0, 1, 0 ) )


# Organzing needed data into a new dataframe
stats <- messy_stats %>%
    select(game_str, play_id, run_dist, OF_dist, run_speed, OF_momentum_home, OF_momentum_side, home_dist, going, run_min_catch_time, catch_before_score)


# Used the runner and catcher info here to determine whether there is a chance the runner was out, and animating those plays to determine the chance they were out, did this info in Excel

final_2nd_stats <- read_csv("final_2nd_stats.csv")
