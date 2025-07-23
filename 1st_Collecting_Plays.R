# Collects all outfielders acquiring ball after it’s hit something
game_events_field_1st <- game_events %>%
    group_by(game_str, play_id) %>%
    # Looking for outfielder acquiring the ball
    filter((player_position %in% c(7,8,9) & event_code == 2) &
               # Where the play before the ball hit the ground / deflected
               (lag(player_position) == 255 & (lag(event_code) == 16 | lag(event_code) == 9 | lag(event_code) == 10))) %>%
    ungroup()
run_on_1st <- game_info %>% 
    filter(!is.na(first_baserunner))
run1st_pos <- player_pos %>%
    # Create composite key
    mutate(key_run = paste(game_str, play_id, timestamp, sep = "_")) %>%
    filter(
        key_run %in% (game_events_field_1st %>%
                          mutate(key_run = paste(game_str, play_id, timestamp, sep = "_")) %>%
                          pull(key_run)),
        player_position == 11
    ) %>%
    # Deselect some unnecessary columns
    dplyr::select(-c("Season", "HomeTeam", "AwayTeam", "Day")) %>%
    collect()

run1st_pos <- run1st_pos %>% rename(runner = player_position)
run1st_pos <- run1st_pos %>% rename(run_x = field_x)
run1st_pos <- run1st_pos %>% rename(run_y = field_y)

game_events_field_1st <- game_events_field_1st %>%
    mutate(key_run = paste(game_str, play_id, timestamp, sep = "_"))
messy_stats_1st <- full_join(game_events_field_1st, run1st_pos[,4:7], by = "key_run")

messy_stats_1st <- messy_stats_1st %>%
    filter(!is.na(runner))
run1st_OF_pos <- player_pos %>%
    # Create composite key
    mutate(key_of = paste(game_str, play_id, timestamp, player_position, sep = "_")) %>%
    filter(
        key_of %in% (messy_stats_1st %>%
            mutate(key_of = paste(game_str, play_id, timestamp, player_position, sep = "_")) %>%
            pull(key_of))
    ) %>%
    # Deselect some unnecessary columns
    dplyr::select(-c("Season", "HomeTeam", "AwayTeam", "Day")) %>%
    collect()

run1st_OF_pos <- run1st_OF_pos %>% rename(OF = player_position)
run1st_OF_pos <- run1st_OF_pos %>% rename(OF_x = field_x)
run1st_OF_pos <- run1st_OF_pos %>% rename(OF_y = field_y)

messy_stats_1st <- messy_stats_1st %>%
    mutate(key_of = paste(game_str, play_id, timestamp, player_position, sep = "_"))
messy_stats_1st <- full_join(messy_stats_1st, run1st_OF_pos[,4:7], by = "key_of")

messy_stats_1st <- messy_stats_1st %>%
    filter(!is.na(OF))
