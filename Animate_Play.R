# animate play function provided by Billy Fryer and David Awosoga during the tutorials and in the discord, thank you

function(game_id, play) {
    
    # Set the specs for the gif we want to create (lower res to make it run quicker)
    options(gganimate.dev_args = list(width = 3, height = 3, units = 'in', res = 120))
    
    # sometimes the frames per second at different stadiums can vary (30 fps vs 50 fps)
    # this finds an even rounding interval and calculates fps from the data explicitly
    fps <- player_pos %>%
        filter(game_str == game_id, play_id == play) %>%
        collect() %>%
        mutate(player_position = as.numeric(player_position)) %>%
        filter(player_position < 14) %>% # only filter for players 
        mutate(across(c(timestamp, field_x, field_y), as.numeric)) %>%
        mutate(fps = timestamp - lag(timestamp), .by = "player_position")  %>%
        count(fps) %>% slice_max(n) %>% pull(fps)
    
    time_of_pitch = game_events %>%
        filter(game_str == game_id, play_id == play, event_code == 1) %>%
        collect() %>%
        pull(timestamp)
    
    # Combine data into one data frame
    tracking_data <- player_pos %>%
        # start with player position data
        filter(game_str == game_id, play_id == play) %>%
        collect() %>%
        mutate(player_position = as.numeric(player_position)) %>%
        filter(player_position < 14) %>%
        mutate(type = if_else(player_position %in% c(10:13), "batter", "fielder"),
               position_z = NA
        ) %>%
        rename(position_x = field_x, position_y = field_y) %>%
        # add ball position data
        bind_rows(
            (ball_pos %>%
                 filter(game_str == game_id, play_id == play) %>%
                 collect() %>%
                 rename(position_x = ball_position_x,
                        position_y = ball_position_y,
                        position_z = ball_position_z) %>%
                 mutate(type = "ball", player_position = NA))
        )  %>%
        mutate(across(c(timestamp, position_x, position_y, position_z), as.numeric)) %>%
        arrange(timestamp) %>%
        # align timestamps to account for mechanical measurement error
        mutate(timestamp_adj = plyr::round_any(timestamp, fps)) %>%
        # trim the animation to start when the pitch is thrown
        filter(timestamp >= time_of_pitch) %>%
        mutate(frame_id = match(timestamp_adj, unique(timestamp_adj)))
    
    # make field design
    p <-  geom_baseball(league = "MiLB") +
        geom_point(data = tracking_data %>%
                       filter(type != "ball"),
                   aes(x = position_x, y = position_y, fill = type),
                   shape = 21, size = 3,
                   show.legend = F) +
        geom_text(data = tracking_data %>%
                      filter(type == "fielder"),
                  aes(x = position_x, y = position_y, label = player_position),
                  color = "black", size = 2,
                  show.legend = F) +
        geom_point(data = tracking_data %>%
                       filter(type == "ball"),
                   aes(x = position_x, y = position_y,
                       size = position_z),
                   fill = "white",
                   shape = 21,
                   show.legend = F) +
        transition_time(frame_id) +
        annotate("text", x = c(150, 0), y = c(10, 400), color = "white",
                 label = c(paste("Play:", play), paste("Game ID:", game_id))) +
        shadow_wake(0.1, exclude_layer = c(1:16))
    
    max_frame = max(tracking_data$frame_id)
    
    p2 = animate(p, fps = fps, nframes = max_frame)
    
    return(p2)
}
