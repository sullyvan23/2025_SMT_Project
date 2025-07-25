# Getting runner position ~0.5 seconds back (500 for plays where they go by 50 to 100, 495 for plays where they go by 33)
run2nd_lag_pos <- player_pos %>%
    # Create composite key
    mutate(key_run = paste(game_str, play_id, timestamp + 500, sep = "_")) %>%
    filter(
        key_run %in% messy_stats$key_run,
        player_position == 12
    ) %>%
    # Deselect some unnecessary columns
    select(-c("Season", "HomeTeam", "AwayTeam", "Day")) %>%
    collect()

run2nd_lag_pos_2.0 <- player_pos %>%
    # Create composite key
    mutate(key_run = paste(game_str, play_id, timestamp + 495, sep = "_")) %>%
    filter(
        key_run %in% messy_stats$key_run,
        player_position == 12
    ) %>%
    # Deselect some unnecessary columns
    select(-c("Season", "HomeTeam", "AwayTeam", "Day")) %>%
    collect()


# Combining lag positions
run2nd_lag_pos <- rbind(run2nd_lag_pos_1.0, run2nd_lag_pos_2.0)

run2nd_lag_pos <- run2nd_lag_pos %>% rename(lag_timestamp = timestamp)
run2nd_lag_pos <- run2nd_lag_pos %>% rename(lag_run_x = field_x)
run2nd_lag_pos <- run2nd_lag_pos %>% rename(lag_run_y = field_y)


# Join to messy stats which has current position data
messy_stats <- full_join(messy_stats, run2nd_lag_pos[,c(3,5:7)], by = "key_run")


# Getting speed over those ~0.5 seconds in ft/sec
messy_stats <- messy_stats %>%
    mutate( run_speed = ( sqrt((run_x - lag_run_x)^2 + (run_y - lag_run_y)^2) /  ((timestamp - lag_timestamp)/1000) ) )
