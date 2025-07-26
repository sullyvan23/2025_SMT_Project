# Getting position of outfielder ~0.5 seconds before they acquire the ball
messy_stats_1st <- messy_stats_1st %>%
    mutate(lag_key_of = paste(game_str, play_id, lag_timestamp, player_position, sep = "_"))

run1st_OF_lag_pos <- player_pos %>%
    # Create composite key
    mutate(lag_key_of = paste(game_str, play_id, timestamp, player_position, sep = "_")) %>%
    filter(
        lag_key_of %in% messy_stats_1st$lag_key_of
    ) %>%
    # Deselect some unnecessary columns
    dplyr::select(-c("Season", "HomeTeam", "AwayTeam", "Day")) %>%
    collect()

run1st_OF_lag_pos <- run1st_OF_lag_pos %>% rename(lag_OF_x = field_x)
run1st_OF_lag_pos <- run1st_OF_lag_pos %>% rename(lag_OF_y = field_y)


# Join lag positions to messy stats where position at ball acquisition data is
messy_stats_1st <- full_join(messy_stats_1st, run1st_OF_lag_pos[,5:7], by = "lag_key_of")


# Getting outfielders velocity in x and y directins in ft/sec
messy_stats_1st <- messy_stats_1st %>%
    mutate(
        OF_x_velo = ( (OF_x - lag_OF_x) / ((timestamp - lag_timestamp)/1000) ),
        OF_y_velo = ( (OF_y - lag_OF_y) / ((timestamp - lag_timestamp)/1000) )
    )


# Geting projection of speeds in x and y onto home and side vectors
messy_stats_1st <- messy_stats_1st %>%
    rowwise() %>%
    mutate(
        # projecting velo vector onto position vector
        OF_home_mom = list(OF_proj(OF_x_velo, OF_y_velo, OF_x, OF_y))
        ,
        #projecting velo vector onto perpendicular vector of position
        OF_side_mom = list(OF_proj(OF_x_velo, OF_y_velo, -OF_y, OF_x))
    ) %>%
    ungroup()

messy_stats_1st <- messy_stats_1st %>%
    mutate(
        OF_home_x = sapply(OF_home_mom, `[`, 1) ,
        OF_home_y = sapply(OF_home_mom, `[`, 2) , 
        OF_side_x = sapply(OF_side_mom, `[`, 1) ,
        OF_side_y = sapply(OF_side_mom, `[`, 2)   
    )


# Getting home and side momentum (for home momentum, positive = going towards home, negative = away from home) (side momentum always positive)
messy_stats_1st <- messy_stats_1st %>%
    mutate(
        OF_momentum_home = ifelse( OF_home_y >= 0,
            -sqrt(OF_home_x^2 + OF_home_y^2) ,
            sqrt(OF_home_x^2 + OF_home_y^2) ) ,
        OF_momentum_side = sqrt(OF_side_x^2 + OF_side_y^2)
    )
