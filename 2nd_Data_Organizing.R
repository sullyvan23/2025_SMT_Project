library(dplyr)

stats <- stats %>%
    mutate(play_key = paste(game_str, play_id, sep = "_"))

stats_2 <- stats %>%
    distinct(play_key, .keep_all = TRUE)

final_2nd_stats_2 <- final_2nd_stats %>%
     distinct(play_key, .keep_all = TRUE)

final_2nd_data <- left_join(final_2nd_stats_2, stats_2, by = "play_key")

final_2nd_data <- final_2nd_data %>%
    dplyr::select("game_str.x", "play_id.x", "play_key", "run_dist.y", "OF_dist.y", "run_speed.y", "top_speed", "OF_momentum_home.y", "OF_momentum_side.y", "score_chance", "safe_out", "go_safe_out")

final_2nd_data <- final_2nd_data %>% rename(game_str = game_str.x)
final_2nd_data <- final_2nd_data %>% rename(play_id = play_id.x)
final_2nd_data <- final_2nd_data %>% rename(run_dist = run_dist.y)
final_2nd_data <- final_2nd_data %>% rename(OF_dist = OF_dist.y)
final_2nd_data <- final_2nd_data %>% rename(run_speed = run_speed.y)
final_2nd_data <- final_2nd_data %>% rename(OF_momentum_home = OF_momentum_home.y)
final_2nd_data <- final_2nd_data %>% rename(OF_momentum_side = OF_momentum_side.y)

went_2nd_data <- final_2nd_data %>% filter(!is.na(safe_out))
