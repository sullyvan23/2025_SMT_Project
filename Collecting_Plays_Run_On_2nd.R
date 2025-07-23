# Collects all balls that hit the ground then are acquired by an outfielder
game_events_ground <- game_events %>%
    # Group by game and play
    group_by(game_str, play_id) %>%
    # Looking for ball on ground
    filter((player_position == 255 & event_code == 16) & 
               # Where the next row is the outfielder acquiring the ball
               (lead(player_position) %in% c(7,8,9) & lead(event_code) == 2)) %>%
    ungroup()

# Collects all outfielders acquiring ball after it’s hit ground
game_events_field <- game_events %>%
    group_by(game_str, play_id) %>%
    # Looking for outfielder acquiring the ball
    filter((player_position %in% c(7,8,9) & event_code == 2) &
               # Where the play before the ball hit the ground
               (lag(player_position) == 255 & lag(event_code) == 16)) %>%
    ungroup()

# Combines both data frames and sorts chronologically
game_events_bounce_to_OF <- bind_rows(game_events_ground, game_events_field) %>%
    arrange(game_str, play_id, timestamp)

run_on_2nd <- game_info %>% 
    filter(!is.na(second_baserunner))

ball_ground_run_on_2nd <- game_events_bounce_to_OF %>%
    # Join the game_info to the bounce to OF game events
    left_join(run_on_2nd, by = join_by("game_str", "at_bat", "play_per_game", "Season", "HomeTeam", "AwayTeam", "Day")) %>%
    # Select only necessary columns
    select(game_str, play_id, at_bat, play_per_game, timestamp, 
           player_position, event_code, second_baserunner) %>%
    filter(!is.na(second_baserunner))

run2nd_fielded <- ball_ground_run_on_2nd %>%
    filter(player_position != 255) %>%
    collect()
