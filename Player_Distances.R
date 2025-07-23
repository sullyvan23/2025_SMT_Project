run2nd_pos <- player_pos %>%
    # Create composite key
    mutate(key = paste(game_str, play_id, timestamp, sep = "_")) %>%
    filter(
        key %in% (run2nd_fielded %>%
            mutate(key = paste(game_str, play_id, timestamp, sep = "_")) %>%
            pull(key)),
        player_position == 12
    ) %>%
    # Deselect some unnecessary columns
    select(-c("Season", "HomeTeam", "AwayTeam", "Day")) %>%
    collect()

run2nd_OF_pos <- player_pos %>%
    # Create composite key
    mutate(key = paste(game_str, play_id, timestamp, player_position, sep = "_")) %>%
    filter(
        key %in% (run2nd_fielded %>%
            mutate(key = paste(game_str, play_id, timestamp, player_position, sep = "_")) %>%
            pull(key)),
    ) %>%
    # Deselect some unnecessary columns
    select(-c("Season", "HomeTeam", "AwayTeam", "Day")) %>%
    collect()

run2nd_fielded <- run2nd_fielded %>%
 mutate(key_run = paste(game_str, play_id, timestamp, sep = "_"))

run2nd_fielded <- run2nd_fielded %>%
    mutate(key_of = paste(game_str, play_id, timestamp, player_position, sep = "_"))

run2nd_pos <- run2nd_pos %>% rename(key_run = key)
run2nd_fielded <- run2nd_fielded %>% rename(key_run = key_run)
run2nd_OF_pos <- run2nd_OF_pos %>% rename(key_of = key)
run2nd_fielded <- run2nd_fielded %>%
    mutate(key_of = paste(game_str, play_id, timestamp, player_position, sep = "_"))

run2nd_stats <- full_join(run2nd_fielded, run2nd_OF_pos, by = "key_of")

run2nd_stats <- full_join(run2nd_stats, run2nd_pos, by = "key_run")

run2nd_stats <- run2nd_stats %>% rename(OF_x = field_x.x)
run2nd_stats <- run2nd_stats %>% rename(OF_y = field_y.x)
run2nd_stats <- run2nd_stats %>% rename(run_x = field_x.y)
run2nd_stats <- run2nd_stats %>% rename(run_y = field_y.y)

run2nd_stats <- run2nd_stats %>%
    mutate(OF_dist = sqrt(OF_x^2 + OF_y^2))

run2nd_stats <- run2nd_stats %>%
    mutate(run_dist = ifelse( (run_x > 0 | run_y < 0) , 
        0 ,
        ifelse( (run_y > 90/sqrt(2)) , 
            (90 + sqrt( (run_x + (90/sqrt(2)))^2 + (run_y - (90/sqrt(2)))^2 ) ) , 
            sqrt(run_x^2 + run_y^2) ) 
                ) )
