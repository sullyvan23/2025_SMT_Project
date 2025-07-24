baserunner_data <- player_pos %>%
    # Create composite key
    mutate(play_key = paste(game_str, play_id, sep = "_")) %>%
    filter(
        play_key %in% all_final_data$play_key,
        player_position >= 11, 
        player_position <= 13 
    ) %>%
    collect()

baserunner_data <- baserunner_data %>%
    mutate(run_key = paste(play_key, player_position, sep = "_"))

baserunner_data <- baserunner_data %>%
    distinct(run_key, .keep_all = TRUE)

all_final_data_2 <- all_final_data %>%
    mutate(baserunners = paste(
        ifelse(paste(play_key, "11", sep = "_") %in% baserunner_data$run_key, "1", "_"),
        ifelse(paste(play_key, "12", sep = "_") %in% baserunner_data$run_key, "2", "_"),
        ifelse(paste(play_key, "13", sep = "_") %in% baserunner_data$run_key, "3", "_"),
        sep = ""
    ))

next_baserunner_data <- player_pos %>%
    # Create composite key
    mutate(play_key = paste(game_str, play_id - 1, sep = "_")) %>%
    filter(
        play_key %in% all_final_data$play_key,
        player_position >= 11, 
        player_position <= 13 
    ) %>%
    collect()

next_baserunner_data <- next_baserunner_data %>%
    mutate(run_key = paste(play_key, player_position, sep = "_"))

next_baserunner_data <- next_baserunner_data %>%
    distinct(run_key, .keep_all = TRUE)

all_final_data_2 <- all_final_data_2 %>%
    mutate(next_baserunners = paste(
        ifelse(paste(play_key, "11", sep = "_") %in% next_baserunner_data $run_key, "1", "_"),
        ifelse(paste(play_key, "12", sep = "_") %in% next_baserunner_data $run_key, "2", "_"),
        ifelse(paste(play_key, "13", sep = "_") %in% next_baserunner_data $run_key, "3", "_"),
        sep = ""
    ))

all_final_data_3 <- all_final_data_2 %>%
    filter( ifelse( ((baserunners == "1__" & next_baserunners == "12_") |  
    (baserunners == "12_" & (next_baserunners == "12_" | next_baserunners == "123")) | 
    (baserunners == "1_3" & next_baserunners == "12_") | 
    (baserunners == "123" & (next_baserunners == "12_" | next_baserunners == "123"))) & 
    scoring_from == 1 , 1, 0 ) != 1 )

all_final_data_3 <- all_final_data_3 %>%
    filter(run_dist <= 180)
