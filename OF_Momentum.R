messy_stats <- messy_stats %>%
    mutate(lag_key_of = paste(game_str, play_id, lag_timestamp, player_position, sep = "_"))

run2nd_OF_lag_pos <- player_pos %>%
    # Create composite key
    mutate(lag_key_of = paste(game_str, play_id, timestamp, player_position, sep = "_")) %>%
    filter(
        lag_key_of %in% messy_stats$lag_key_of
    ) %>%
    # Deselect some unnecessary columns
    select(-c("Season", "HomeTeam", "AwayTeam", "Day")) %>%
    collect()

run2nd_OF_lag_pos <- run2nd_OF_lag_pos %>% rename(lag_OF_x = field_x)
run2nd_OF_lag_pos <- run2nd_OF_lag_pos %>% rename(lag_OF_y = field_y)

messy_stats <- full_join(messy_stats, run2nd_OF_lag_pos[,5:7], by = "lag_key_of")

messy_stats <- messy_stats %>%
    mutate(
        OF_x_velo = ( (OF_x - lag_OF_x) / ((timestamp - lag_timestamp)/1000) ),
        OF_y_velo = ( (OF_y - lag_OF_y) / ((timestamp - lag_timestamp)/1000) )
    )

OF_proj <- function(x, y, a, b) {
    
    A <- c(x,y)
    B <- c(a,b)
    
    # the projection of A onto B
    proj_A_on_B <- (sum(A * B) / sum(B^2)) * B
    
    return(proj_A_on_B)
 }

messy_stats <- messy_stats %>%
    rowwise() %>%
    mutate(
        # projecting velo vector onto position vector
        OF_home_mom = list(OF_proj(OF_x_velo, OF_y_velo, OF_x, OF_y))
        ,
        #projecting velo vector onto perpendicular vector of position
        OF_side_mom = list(OF_proj(OF_x_velo, OF_y_velo, -OF_y, OF_x))
    ) %>%
    ungroup()
messy_stats <- messy_stats %>%
    mutate(
        OF_home_x = sapply(OF_home_mom, `[`, 1) ,
        OF_home_y = sapply(OF_home_mom, `[`, 2) , 
        OF_side_x = sapply(OF_side_mom, `[`, 1) ,
        OF_side_y = sapply(OF_side_mom, `[`, 2)   
    )

messy_stats <- messy_stats %>%
    mutate(
        OF_momentum_home = ifelse( OF_home_y >= 0,
            -sqrt(OF_home_x^2 + OF_home_y^2) ,
            sqrt(OF_home_x^2 + OF_home_y^2) ) ,
        OF_momentum_side = sqrt(OF_side_x^2 + OF_side_y^2)
    )

messy_stats <- messy_stats %>%
    select(-OF_home_mom, -OF_side_mom)
