final_1st_data <- final_1st_data %>%
    mutate(scoring_from = 1)

final_2nd_data <- final_2nd_data %>%
    mutate(scoring_from = 2)

all_final_data <- rbind(final_1st_data, final_2nd_data)

ball_hit <- game_events %>%
    # Create composite key
    mutate(play_key = paste(game_str, play_id, sep = "_")) %>%
    filter(
        play_key %in% all_final_data$play_key,
        event_code == 4
    ) %>%
    collect()

all_final_data <- full_join(all_final_data, ball_hit[,c(7,12)], by = "play_key")

all_final_data <- all_final_data %>%
    filter(!is.na(event_code))

all_final_data <- all_final_data[,1:11]

all_went_data <- all_final_data %>%
    filter(!is.na(safe_out))
