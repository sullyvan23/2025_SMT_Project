library(dplyr)

# Using player position to determine baserunners because of innaccuracies in game_info
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


# Creating baserunner situations from data
all_final_data_2 <- all_final_data %>%
    mutate(baserunners = paste(
        ifelse(paste(play_key, "11", sep = "_") %in% baserunner_data$run_key, "1", "_"),
        ifelse(paste(play_key, "12", sep = "_") %in% baserunner_data$run_key, "2", "_"),
        ifelse(paste(play_key, "13", sep = "_") %in% baserunner_data$run_key, "3", "_"),
        sep = ""
    ))


# Seeing what baserunner situations looked like on next play for helping determine run expectancies
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


# Filtered out plays where it was looking at a runner on first and the player didn't even advance to third
all_final_data_3 <- all_final_data_2 %>%
    filter( ifelse( ((baserunners == "1__" & next_baserunners == "12_") |  
    (baserunners == "12_" & (next_baserunners == "12_" | next_baserunners == "123")) | 
    (baserunners == "1_3" & next_baserunners == "12_") | 
    (baserunners == "123" & (next_baserunners == "12_" | next_baserunners == "123"))) & 
    scoring_from == 1 , 1, 0 ) != 1 )


# Filtering out plays where the runner obviously was never gonna be sent home, largest run_dist that actually went was around 170 feet
all_final_data_3 <- all_final_data_3 %>%
    filter(run_dist <= 180)


# Did more work in excel to get good baserunning situations
