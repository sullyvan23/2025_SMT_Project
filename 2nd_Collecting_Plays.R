library(dplyr)

# Collecing plays where outfielder acquiring ball after it’s hit ground
game_events_field <- game_events %>%
    group_by(game_str, play_id) %>%
    # Looking for outfielder acquiring the ball
    filter((player_position %in% c(7,8,9) & event_code == 2) &
               # Where the play before the ball hit the ground
               (lag(player_position) == 255 & lag(event_code) == 16)) %>%
    ungroup()


# Collecting plays with a runner on 2nd
run_on_2nd <- game_info %>% 
    filter(!is.na(second_baserunner))

run2nd_pos <- player_pos %>%
    # Create composite key
    mutate(key_run = paste(game_str, play_id, timestamp, sep = "_")) %>%
    filter(
        key_run %in% (game_events_field %>%
                          mutate(key_run = paste(game_str, play_id, timestamp, sep = "_")) %>%
                          pull(key_run)),
        player_position == 12
    ) %>%
    # Deselect some unnecessary columns
    select(-c("Season", "HomeTeam", "AwayTeam", "Day")) %>%
    collect()

run2nd_pos <- run2nd_pos %>% rename(runner = player_position)
run2nd_pos <- run2nd_pos %>% rename(run_x = field_x)
run2nd_pos <- run2nd_pos %>% rename(run_y = field_y)

game_events_field <- game_events_field %>%
    mutate(key_run = paste(game_str, play_id, timestamp, sep = "_"))


# Combining runner on 2nd and outfielder fielding ball to get the plays where both happened
messy_stats <- full_join(game_events_field, run2nd_pos[,4:7], by = "key_run")

messy_stats <- messy_stats %>%
    filter(!is.na(runner))


# Getting positions of outfielder and runner at the time the ball is fielded
run2nd_OF_pos <- player_pos %>%
    # Create composite key
    mutate(key_of = paste(game_str, play_id, timestamp, player_position, sep = "_")) %>%
    filter(
        key_of %in% (messy_stats %>%
            mutate(key_of = paste(game_str, play_id, timestamp, player_position, sep = "_")) %>%
            pull(key_of))
    ) %>%
    # Deselect some unnecessary columns
    select(-c("Season", "HomeTeam", "AwayTeam", "Day")) %>%
    collect()

run2nd_OF_pos <- run2nd_OF_pos %>% rename(OF = player_position)
run2nd_OF_pos <- run2nd_OF_pos %>% rename(OF_x = field_x)
run2nd_OF_pos <- run2nd_OF_pos %>% rename(OF_y = field_y)

messy_stats <- messy_stats %>%
    mutate(key_of = paste(game_str, play_id, timestamp, player_position, sep = "_"))

messy_stats <- full_join(messy_stats, run2nd_OF_pos[,4:7], by = "key_of")

messy_stats <- messy_stats %>%
    filter(!is.na(OF))
