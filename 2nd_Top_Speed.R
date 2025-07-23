final_2nd_stats <- final_2nd_stats %>%
    mutate(play_key = paste(game_str, play_id, sep = "_")) %>%
    relocate(play_key, .after = play_id)

run_max <- player_pos %>%
    # Create composite key
    mutate(play_key = paste(game_str, play_id, sep = "_")) %>%
    filter(
        play_key %in% final_2nd_stats$play_key,
        player_position == 12
    ) %>%
    collect()

run_max <- run_max %>%
    mutate(time_key = paste(play_key, timestamp, sep="_"))

lag_run_max1 <- run_max

lag_run_max1 <- lag_run_max1 %>%
    mutate(time_key = paste(play_key, timestamp+500, sep="_"))

lag_run_max2 <- run_max

lag_run_max2 <- lag_run_max2 %>%
    mutate(time_key = paste(play_key, timestamp+495, sep="_"))

lag_run_max <- rbind(lag_run_max1, lag_run_max2)

run_max <- full_join(run_max, lag_run_max[,c(3,5:6,12)], by="time_key")

run_max <- run_max %>%
    mutate(max_speed = sqrt( (field_x.x - field_x.y)^2 + (field_y.x - field_y.y)^2 ) / ((timestamp.x - timestamp.y) /1000) )

run_max <- run_max %>%
    filter(!is.na(max_speed))

run_top_speed <- run_max %>%
    group_by(play_key) %>%
    summarise(top_speed = max(max_speed), .groups = "drop") %>%
    ungroup()

final_2nd_stats <- full_join(final_2nd_stats, run_top_speed, by="play_key") %>%
    relocate(top_speed, .after = run_speed)
