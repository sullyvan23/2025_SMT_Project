final_1st_stats <- final_1st_stats %>%
    mutate(play_key = paste(game_str, play_id, sep = "_")) %>%
    relocate(play_key, .after = play_id)


# Gathering all positions of runner over play
run_max_1st <- player_pos %>%
    # Create composite key
    mutate(play_key = paste(game_str, play_id, sep = "_")) %>%
    filter(
        play_key %in% final_1st_stats$play_key,
        player_position == 11
    ) %>%
    collect()

run_max_1st <- run_max_1st %>%
    mutate(time_key = paste(play_key, timestamp, sep="_"))


# Attaching where the player was ~0.5 seconds ago 
lag_run_max1_1st <- run_max_1st[,1:11]

lag_run_max1_1st <- lag_run_max1_1st %>%
    mutate(time_key = paste(play_key, as.numeric(timestamp) + 500, sep="_"))

lag_run_max2_1st <- run_max_1st[,1:11]

lag_run_max2_1st <- lag_run_max2_1st %>%
    mutate(time_key = paste(play_key, as.numeric(timestamp) + 495, sep="_"))

lag_run_max_1st <- rbind(lag_run_max1_1st, lag_run_max2_1st)

run_max_1st <- full_join(run_max_1st, lag_run_max_1st[,c(3,5:6,12)], by="time_key")


# Calculating speed over the ~0.5 seconds for all points applicable
run_max_1st <- run_max_1st %>%
    mutate(max_speed = sqrt( (field_x.x - field_x.y)^2 + (field_y.x - field_y.y)^2 ) / ((timestamp.x - timestamp.y) /1000) )

run_max_1st <- run_max_1st %>%
    filter(!is.na(max_speed))


# Taking the greatest speed from every play
run_top_speed_1st <- run_max_1st %>%
    group_by(play_key) %>%
    summarise(top_speed = max(max_speed), .groups = "drop") %>%
    ungroup()

final_1st_stats <- full_join(final_1st_stats, run_top_speed_1st, by="play_key") %>%
    relocate(top_speed, .after = run_speed)
