# Getting runner position ~0.5 seconds back (500 for plays where they go by 50 to 100, 495 for plays where they go by 33)
run1st_lag_pos <- player_pos %>%
    # Create composite key
    mutate(key_run = paste(game_str, play_id, timestamp + 500, sep = "_")) %>%
    filter(
        key_run %in% messy_stats_1st$key_run,
        player_position == 11
    ) %>%
    # Deselect some unnecessary columns
    dplyr::select(-c("Season", "HomeTeam", "AwayTeam", "Day")) %>%
    collect()

run1st_lag_pos_2.0 <- player_pos %>%
    # Create composite key
    mutate(key_run = paste(game_str, play_id, timestamp + 495, sep = "_")) %>%
    filter(
        key_run %in% messy_stats_1st$key_run,
        player_position == 11
    ) %>%
    # Deselect some unnecessary columns
    dplyr::select(-c("Season", "HomeTeam", "AwayTeam", "Day")) %>%
    collect()


# Combining lag positions
run1st_lag_pos <- rbind(run1st_lag_pos, run1st_lag_pos_2.0)

run1st_lag_pos <- run1st_lag_pos %>% rename(lag_timestamp = timestamp)
run1st_lag_pos <- run1st_lag_pos %>% rename(lag_run_x = field_x)
run1st_lag_pos <- run1st_lag_pos %>% rename(lag_run_y = field_y)


# Join to messy stats which has current position data
messy_stats_1st <- full_join(messy_stats_1st , run1st_lag_pos[,c(3,5:7)], by = "key_run")


# Getting speed over those ~0.5 seconds in ft/sec
messy_stats_1st <- messy_stats_1st %>%
    mutate( run_speed = ( sqrt((run_x - lag_run_x)^2 + (run_y - lag_run_y)^2) /  ((timestamp - lag_timestamp)/1000) ) )
