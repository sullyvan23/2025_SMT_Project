# This is all my unorganized code with everything I did, including tests and tries of things that ended up leading to nothing or were just tests of concepts
# I organized everything I needed into the numbered files but in case there is something missing link or code or something it should be here, although hopefully this is not needed

library(tidyverse)
library(arrow)
library(sportyR)
player_pos <- arrow::open_csv_dataset(paste0(data_directory,"/player_pos"), 
                                      partitioning = c("HomeTeam", "AwayTeam", "Season", "Day"),
                                      hive_style = F, 
                                      unify_schemas = T, 
                                      na = c("", "NA", "NULL", NA, "\\N"))

ball_pos <- arrow::open_csv_dataset(paste0(data_directory,"/ball_pos"), 
                                    partitioning = c("HomeTeam", "AwayTeam", "Season", "Day"),
                                    hive_style = F, 
                                    unify_schemas = T, 
                                    na = c("", "NA", "NULL", NA, "\\N"))

player_pos <- player_pos %>%
  collect()

ball_pos <- ball_pos %>%
  collect()
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
    left_join(run_on_2nd, by = join_by("game_str", "at_bat", "play_per_game", 
                                              "Season", "HomeTeam","AwayTeam", "Day")) %>%
    # Select only necessary columns to preserve memory
    select(game_str, play_id, at_bat, play_per_game, timestamp, 
           player_position, event_code, second_baserunner) %>%
    # Only want when the centerfielder is player1
    filter(!is.na(second_baserunner))
run2nd_fielded <- ball_ground_run_on_2nd %>%
    filter(player_position != 255) %>%
    collect()
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

Baserunner distance to home (know they start at 2nd base)
2nd base is ( 0 , 90*sqrt(2) )			( 0 , 127.3 )
3rd base is ( -90/sqrt(2) , 90/sqrt(2))		( -63.6 , 63.6 )
Home is ( 0 , 0 )
3rd to home is 90 feet
if( (x > 0) OR (y < 0) )			0
else if( y > 63.6 )			dist to 3rd + 90
else					dist to home

run2nd_stats <- run2nd_stats %>%
    mutate(run_dist = ifelse( (run_x > 0 | run_y < 0) , 
        0 ,
        ifelse( (run_y > 90/sqrt(2)) , 
            (90 + sqrt( (run_x + (90/sqrt(2)))^2 + (run_y - (90/sqrt(2)))^2 ) ) , 
            sqrt(run_x^2 + run_y^2) ) 
                ) )
run2nd_lag_pos <- player_pos %>%
    # Create composite key
    mutate(key = paste(game_str, play_id, lead(timestamp, n=9), sep = "_")) %>%
    filter(
        key %in% run2nd_fielded$key_run,
        player_position == 12
    ) %>%
    # Deselect some unnecessary columns
    select(-c("Season", "HomeTeam", "AwayTeam", "Day")) %>%
    collect()
run2nd_OF_lag_pos <- player_pos %>%
    # Create composite key
    mutate(key = paste(game_str, play_id, lead(timestamp, n=13), player_position, sep = "_")) %>%
    filter(
        key %in% run2nd_fielded$key_of
    ) %>%
    # Deselect some unnecessary columns
    select(-c("Season", "HomeTeam", "AwayTeam", "Day")) %>%
    collect()
run2nd_lag_pos <- run2nd_lag_pos %>% rename(key_run = key)
run2nd_OF_lag_pos <- run2nd_OF_lag_pos %>% rename(key_of = key)
run2nd_stats <- full_join(run2nd_stats, run2nd_OF_lag_pos, by = "key_of")
run2nd_stats <- full_join(run2nd_stats, run2nd_lag_pos, by = "key_run")
run2nd_stats <- run2nd_stats %>% rename(lag_OF_x = field_x.x)
run2nd_stats <- run2nd_stats %>% rename(lag_OF_y = field_y.x)
run2nd_stats <- run2nd_stats %>% rename(lag_run_x = field_x.y)
run2nd_stats <- run2nd_stats %>% rename(lag_run_y = field_y.y)
run2nd_stats <- run2nd_stats %>% rename(lag_timestamp = timestamp)
run2nd_stats <- run2nd_stats %>% rename(timestamp = timestamp.x)
run2nd_stats <- run2nd_stats %>%
    mutate( run_speed = ( sqrt((run_x - lag_run_x)^2 + (run_y - lag_run_y)^2) /  ((timestamp - lag_timestamp)/1000) ) )
run2nd_stats <- run2nd_stats %>%
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

run2nd_stats <- run2nd_stats %>%
    rowwise() %>%
    mutate(
        # projecting velo vector onto position vector
        OF_home_mom = list(OF_proj(OF_x_velo, OF_y_velo, OF_x, OF_y))
        ,
        #projecting velo vector onto perpendicular vector of position
        OF_side_mom = list(OF_proj(OF_x_velo, OF_y_velo, -OF_y, OF_x))
    ) %>%
    ungroup()
run2nd_stats <- run2nd_stats %>%
    mutate(
        OF_home_x = sapply(OF_home_mom, `[`, 1) ,
        OF_home_y = sapply(OF_home_mom, `[`, 2) , 
        OF_side_x = sapply(OF_side_mom, `[`, 1) ,
        OF_side_y = sapply(OF_side_mom, `[`, 2)   
    )
run2nd_stats <- run2nd_stats %>%
    mutate(
        OF_momentum_home = ifelse( OF_home_y >= 0,
            -sqrt(OF_home_x^2 + OF_home_y^2) ,
            sqrt(OF_home_x^2 + OF_home_y^2) ) ,
        OF_momentum_side = sqrt(OF_side_x^2 + OF_side_y^2)
    )
run2nd_stats <- run2nd_stats %>%
    select(-OF_home_mom, -OF_side_mom)

write.csv(run2nd_stats, "run2nd_stats.csv", row.names = FALSE)
catcher_rec <- game_events %>%
    # Create composite key
    mutate(catcher_key = paste(game_str, play_id, sep = "_")) %>%
    filter(
        catcher_key %in% (run2nd_stats %>%
                mutate(catcher_key = paste(game_str, play_id, sep = "_")) %>%
                pull(catcher_key)),
        player_position == 2 ,
        event_code == 2
        ) %>%
    # Deselect some unnecessary columns
    select(-c("Season", "HomeTeam", "AwayTeam", "Day", "at_bat", "play_per_game")) %>%
    collect()
run2nd_stats <- run2nd_stats %>%
    mutate(catcher_key = paste(game_str, play_id, sep = "_"))

run2nd_stats <- full_join(run2nd_stats, catcher_rec, by = "catcher_key")
catcher_runner <- player_pos %>%
    # Create composite key
    mutate(catcher_key = paste(game_str, play_id, sep = "_")) %>%
    filter(
        catcher_key %in% catcher_rec$catcher_key,
        player_position %in% c(2,12)
    ) %>%
    # Deselect some unnecessary columns
    select(-c("Season", "HomeTeam", "AwayTeam", "Day")) %>%
    collect()
catcher_runner <- full_join(catcher_runner , catcher_rec %>% select(catcher_key, timestamp) , by = "catcher_key")

catcher_runner <- catcher_runner %>% rename(live_timestamp = timestamp.x)
catcher_runner <- catcher_runner %>% rename(catcher_timestamp = timestamp.y)

catcher_runner <- catcher_runner %>%
   mutate(time_diff = live_timestamp - catcher_timestamp)
score_key <- run2nd_stats %>%
    mutate(key = paste(game_str.x, at_bat, second_baserunner, sep = "_")) %>%
    pull(key)

run_go_home <- game_info %>%
    mutate(key = paste(game_str, at_bat - 1, third_baserunner, sep = "_")) %>%
    filter(key %in% score_key) %>%
    select(game_str, at_bat, third_baserunner) %>%
    collect()
> save.image("SMT1.RData")

run_go_home <- run_go_home %>%
    mutate(score_key = paste(game_str, at_bat - 1, sep = "_"))

run2nd_stats <- run2nd_stats %>%
    mutate(score_key = paste(game_str.x, at_bat, sep = "_"))
catcher <- catcher_runner %>%
    filter(player_position == 2)
runner <- catcher_runner %>%
    filter(player_position == 12)

catcher <- catcher %>%
    mutate(key = paste(game_str, play_id, live_timestamp, sep = "_"))
runner <- runner %>%
    mutate(key = paste(game_str, play_id, live_timestamp, sep = "_"))

catcher_runner <- full_join(catcher, runner , by = "key")

catcher_runner <- catcher_runner %>%
    mutate(check = live_timestamp.x - live_timestamp.y)

catcher_runner <- catcher_runner %>% rename(live_timestamp = live_timestamp.x)
catcher_runner <- catcher_runner %>% rename(catcher = player_position.x)
catcher_runner <- catcher_runner %>% rename(catcher_x = field_x.x)
catcher_runner <- catcher_runner %>% rename(catcher_y = field_y.x)
catcher_runner <- catcher_runner %>% rename(runner = player_position.y)
catcher_runner <- catcher_runner %>% rename(runner_x = field_x.y)
catcher_runner <- catcher_runner %>% rename(runner_y = field_y.y)

catcher_runner <- catcher_runner %>% 
    select(-catcher_key.x, -catcher_timestamp.x, -time_diff.x, -key)
catcher_runner <- catcher_runner %>%
    mutate(tag_dist = sqrt( (catcher_x - runner_x)^2 + (catcher_y - runner_y)^2 ))

catcher_runner <- catcher_runner %>%
    # y = 0 is back of home plate, plate is 17 inch square with two corners cut off, so middle of the square is at y = 8.5 inches, y = 8.5/12 feet
    mutate(run_home_dist = sqrt( (runner_x)^2 + (runner_y - (8.5/12))^2 ))
catcher_runner <- catcher_runner %>%
    mutate(within_tag_dist = ifelse(tag_dist <= 3, 1, 0))

catcher_runner <- catcher_runner %>%
    mutate(can_score = ifelse(run_home_dist <=3, 1, 0))

catcher_runner <- catcher_runner %>%
    mutate(has_ball = ifelse(time_diff.y >= 0, 1, 0))
catcher_runner <- catcher_runner %>%
    group_by(catcher_key.y) %>%
    mutate(has_scored = cummax(can_score)) %>%
    ungroup()
catcher_runner <- catcher_runner %>%
    mutate(tagged_out = ifelse((within_tag_dist + has_ball - has_scored == 2), 1, 0))
play_can_score <- catcher_runner %>%
    group_by(catcher_key.y) %>%
    summarise(can_score = max(has_scored), .groups = "drop") %>%
    ungroup()

play_tagged <- catcher_runner %>%
    group_by(catcher_key.y) %>%
    summarise(tagged = max(tagged_out), .groups = "drop") %>%
    ungroup()

score_tag <- full_join(play_can_score, play_tagged, by = "catcher_key.y")

score_tag <- score_tag %>% rename(catcher_key = catcher_key.y)

run2nd_stats <- full_join(run2nd_stats, score_tag, by = "catcher_key")
stats <- run2nd_stats %>% distinct()

stats <- stats %>%
    mutate(play_key = paste(game_str.x, play_id.x, sep = "_"))
all_runner <- player_pos %>%
    # Create composite key
    mutate(play_key = paste(game_str, play_id, sep = "_")) %>%
    filter(
        play_key %in% stats$play_key,
        player_position == 12
    ) %>%
    # Deselect some unnecessary columns
    select(-c("Season", "HomeTeam", "AwayTeam", "Day")) %>%
    collect()
all_runner <- all_runner %>%
    # y = 0 is back of home plate, plate is 17 inch square with two corners cut off, so middle of the square is at y = 8.5 inches, y = 8.5/12 feet
    mutate(run_home_dist = sqrt( (field_x)^2 + (field_y - (8.5/12))^2 ))

all_runner <- all_runner %>%
    mutate(can_score = ifelse(run_home_dist <=3, 1, 0))
scored <- all_runner %>%
    group_by(play_key) %>%
    summarise(can_score = max(can_score), .groups = "drop") %>%
    ungroup()
stats <- full_join(stats, scored , by = "play_key")
stats <- stats %>%
    mutate(sit_key = paste(game_str.x, at_bat.x, sep = "_"))

stats <- stats %>%
    mutate(next_sit_key = paste(game_str.x, at_bat.x+1, sep = "_"))
run_situation <- game_info %>%
    # Create composite key
    mutate(sit_key = paste(game_str, at_bat, sep = "_")) %>%
    filter(
        sit_key %in% stats$sit_key
    ) %>%
    # Deselect some unnecessary columns
    select(sit_key, top_bottom_inning, first_baserunner, second_baserunner, third_baserunner) %>%
    collect()

rm(run_situation)
SAFE_OR_OUT <- stats %>%
    select(game_str.x, play_id.x, at_bat.x, second_baserunner, OF_dist, run_dist, OF_momentum_home, OF_momentum_side, run_speed, at_bat.y, third_baserunner, can_score, tagged.x)
check <- player_pos %>%
    # Create composite key
    mutate(key = paste(game_str, play_id, sep = "_")) %>%
    filter(
        key == "y1_d001_CGA_QEA_289",
        player_position == 12
    ) %>%
    # Deselect some unnecessary columns
    select(-c("Season", "HomeTeam", "AwayTeam", "Day")) %>%
    collect()
SAFE_OR_OUT <- SAFE_OR_OUT %>%
    filter(!is.na(run_dist))

SAFE_OR_OUT <- SAFE_OR_OUT %>%
    filter( !(!is.na(third_baserunner) & is.na(at_bat.y)) )
SAFE_OR_OUT <- SAFE_OR_OUT %>%
    mutate(safe_out = ifelse(is.na(tagged.x), can_score, can_score-tagged.x))
write.csv(SAFE_OR_OUT, "SAFE_OR_OUT.csv", row.names = FALSE)
DID SOME WORK IN EXCEL ON THIS NEW_SAFE_OR_OUT
NEW_SAFE_OR_OUT <- read_csv("NEW_SAFE_OR_OUT.csv")

NEW_SAFE_OR_OUT <- NEW_SAFE_OR_OUT %>%
    select(-c("at_bat.y", "third_baserunner", "can_score", "tagged.x", "safe_out", "bang-bang?"))
NEW_SAFE_OR_OUT <- NEW_SAFE_OR_OUT %>%
    select(-`animate_play("game_str.x", play_id.x)`)
NEW_SAFE_OR_OUT <- NEW_SAFE_OR_OUT %>% rename(safe_chance = 'safe_out%')

NEW_SAFE_OR_OUT <- NEW_SAFE_OR_OUT %>%
    filter(safe_chance > -1)

write.csv(NEW_SAFE_OR_OUT, "NEW_SAFE_OR_OUT.csv", row.names = FALSE)
run2nd_lag_pos_2.0 <- player_pos %>%
    # Create composite key
    mutate(key = paste(game_str, play_id, timestamp+1000, sep = "_")) %>%
    filter(
        key %in% stats$key_run,
        player_position == 12
    ) %>%
    # Deselect some unnecessary columns
    select(-c("Season", "HomeTeam", "AwayTeam", "Day")) %>%
    collect()



library(tidyverse)
library(arrow)
library(sportyR)
# Collects all outfielders acquiring ball after it’s hit ground
game_events_field <- game_events %>%
    group_by(game_str, play_id) %>%
    # Looking for outfielder acquiring the ball
    filter((player_position %in% c(7,8,9) & event_code == 2) &
               # Where the play before the ball hit the ground
               (lag(player_position) == 255 & lag(event_code) == 16)) %>%
    ungroup()
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
messy_stats <- full_join(game_events_field, run2nd_pos[,4:7], by = "key_run")

messy_stats <- messy_stats %>%
    filter(!is.na(runner))
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
Baserunner distance to home (know they start at 2nd base)
2nd base is ( 0 , 90*sqrt(2) )			( 0 , 127.3 )
3rd base is ( -90/sqrt(2) , 90/sqrt(2))		( -63.6 , 63.6 )
Home is ( 0 , 0 )
3rd to home is 90 feet
if( (x > 0) OR (y < 0) )			0
else if( y > 63.6 )			dist to 3rd + 90
else					dist to home

messy_stats <- messy_stats %>%
    mutate(run_dist = ifelse( (run_x > 0 | run_y < 0) , 
        0 ,
        ifelse( (run_y > 90/sqrt(2)) , 
            (90 + sqrt( (run_x + (90/sqrt(2)))^2 + (run_y - (90/sqrt(2)))^2 ) ) , 
            sqrt(run_x^2 + run_y^2) ) 
    ) )

messy_stats <- messy_stats %>%
    mutate(OF_dist = sqrt(OF_x^2 + OF_y^2))
run2nd_lag_pos <- player_pos %>%
    # Create composite key
    mutate(key_run = paste(game_str, play_id, timestamp + 500, sep = "_")) %>%
    filter(
        key_run %in% messy_stats$key_run,
        player_position == 12
    ) %>%
    # Deselect some unnecessary columns
    select(-c("Season", "HomeTeam", "AwayTeam", "Day")) %>%
    collect()

run2nd_lag_pos_2.0 <- player_pos %>%
    # Create composite key
    mutate(key_run = paste(game_str, play_id, timestamp + 495, sep = "_")) %>%
    filter(
        key_run %in% messy_stats$key_run,
        player_position == 12
    ) %>%
    # Deselect some unnecessary columns
    select(-c("Season", "HomeTeam", "AwayTeam", "Day")) %>%
    collect()

run2nd_lag_pos <- rbind(run2nd_lag_pos_1.0, run2nd_lag_pos_2.0)

run2nd_lag_pos <- run2nd_lag_pos %>% rename(lag_timestamp = timestamp)
run2nd_lag_pos <- run2nd_lag_pos %>% rename(lag_run_x = field_x)
run2nd_lag_pos <- run2nd_lag_pos %>% rename(lag_run_y = field_y)

messy_stats <- full_join(messy_stats, run2nd_lag_pos[,c(3,5:7)], by = "key_run")
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
    mutate( run_speed = ( sqrt((run_x - lag_run_x)^2 + (run_y - lag_run_y)^2) /  ((timestamp - lag_timestamp)/1000) ) )

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
messy_stats <- messy_stats %>%
    mutate(play_key = paste(game_str, play_id, sep = "_"))

go_home <- player_pos %>%
    # Create composite key
    mutate(play_key = paste(game_str, play_id, sep = "_")) %>%
    filter(
        play_key %in% messy_stats$play_key,
        player_position == 12
    ) %>%
    # Deselect some unnecessary columns
    select(-c("Season", "HomeTeam", "AwayTeam", "Day")) %>%
    collect()
go_home <- go_home %>%
    mutate(home_dist = sqrt(field_x^2 + field_y^2) )

go_home <- go_home %>%
    mutate(going = ifelse( home_dist <= 45, 1, 0) )
close_home <- go_home %>%
    group_by(play_key) %>%
    slice_min(order_by = home_dist, n = 1, with_ties = FALSE) %>%
    ungroup()

close_home <- close_home %>% rename(run_home_time = timestamp)

messy_stats <- full_join(messy_stats, close_home[,c(3,7:9)], by = "play_key")
catcher_rec <- game_events %>%
    # Create composite key
    mutate(play_key = paste(game_str, play_id, sep = "_")) %>%
    filter(
        play_key %in% messy_stats$play_key ,
        player_position == 2 ,
        event_code == 2
    ) %>%
    # Deselect some unnecessary columns
    select(-c("Season", "HomeTeam", "AwayTeam", "Day", "at_bat", "play_per_game")) %>%
    collect()

catcher_rec <- catcher_rec %>% rename(catch_time = timestamp)
messy_stats <- full_join(messy_stats, catcher_rec[,c(3,6)], by = "play_key")

messy_stats <- messy_stats %>%
    mutate(run_min_catch_time = (run_home_time - catch_time) / 1000)

messy_stats <- messy_stats %>%
    mutate(catch_before_score = ifelse( run_min_catch_time > 0, 1, 0 ) )
stats <- messy_stats %>%
    select(game_str, play_id, run_dist, OF_dist, run_speed, OF_momentum_home, OF_momentum_side, home_dist, going, run_min_catch_time, catch_before_score)
final_2nd_stats <- read_csv("final_2nd_stats.csv")

went_2nd_stats <- final_2nd_stats %>%
    filter(!is.na(safe_out))

went_2nd_stats <- went_2nd_stats %>%
    filter(run_dist > 0)
lda_2nd_stats = lda(went_2nd_stats[, 3:7], grouping = went_2nd_stats$safe_out)

predict_2nd_stats = predict(lda_2nd_stats, went_2nd_stats[, 3:7])

sum(predict_2nd_stats$class == went_2nd_stats$safe_out)
[1] 391
391 of 410 correct
Looks like it only predicted 1 out tho

predict_2nd_stats$posterior

went_2nd_stats$lda1_score_prob <- predict_2nd_stats$posterior[, "1"]
went_2nd_stats_2.0 <- went_2nd_stats %>%
    filter(run_dist >= 75)

lda_2nd_stats_2.0 = lda(went_2nd_stats_2.0[, 3:7], grouping = went_2nd_stats_2.0$safe_out)

predict_2nd_stats_2.0 = predict(lda_2nd_stats_2.0, went_2nd_stats_2.0[, 3:7])

sum(predict_2nd_stats_2.0$class == went_2nd_stats_2.0$safe_out)
[1] 232
232 of 251 correct, didn’t predict any outs

predict_2nd_stats_2.0$posterior

went_2nd_stats_2.0$lda2_score_prob <- predict_2nd_stats_2.0$posterior[, "1"]
final_2nd_stats <- final_2nd_stats %>%
    mutate(go_safe_out = ifelse(score_chance > 0.5, 1, 0))

lda_2nd_stats_3.0 = lda(final_2nd_stats[, 3:7], grouping = final_2nd_stats$go_safe_out)

predict_2nd_stats_3.0 = predict(lda_2nd_stats_3.0, final_2nd_stats[, 3:7])

sum(predict_2nd_stats_3.0$class == final_2nd_stats$go_safe_out)
[1] 544
544 of 599 correct

predict_2nd_stats_3.0$posterior

final_2nd_stats$lda3_score_prob <- predict_2nd_stats_3.0$posterior[, "1"]

went_2nd_stats_3.0 <- final_2nd_stats %>%
    filter(!is.na(safe_out))

went_2nd_stats_3.0 <- went_2nd_stats_3.0[,-14]
Run speeds low from waiting to see if ball drops is messing it up a bit
qda_2nd_stats = qda(final_2nd_stats[, 3:7], grouping = final_2nd_stats$go_safe_out)

predict_2nd_stats_qda = predict(qda_2nd_stats, final_2nd_stats[, 3:7])

sum(predict_2nd_stats_qda$class == final_2nd_stats$go_safe_out)
[1] 533
533 of 599 correct
final_2nd_stats <- final_2nd_stats %>%
    mutate(play_key = paste(game_str, play_id, sep = "_")) %>%
    relocate(play_key, .after = play_id)

run_max <- player_pos %>%
    # Create composite key
    mutate(play_key = paste(game_str, play_id, sep = "_")) %>%
    filter(
        play_key %in% final_2nd_stats$play_key,
        player_position == 12
    ) %>%
    collect()

run_max <- run_max %>%
    mutate(time_key = paste(play_key, timestamp, sep="_"))
lag_run_max1 <- run_max

lag_run_max1 <- lag_run_max1 %>%
    mutate(time_key = paste(play_key, timestamp+500, sep="_"))

lag_run_max2 <- run_max

lag_run_max2 <- lag_run_max2 %>%
    mutate(time_key = paste(play_key, timestamp+495, sep="_"))

lag_run_max <- rbind(lag_run_max1, lag_run_max2)
run_max <- full_join(run_max, lag_run_max[,c(3,5:6,12)], by="time_key")

run_max <- run_max %>%
    mutate(max_speed = sqrt( (field_x.x - field_x.y)^2 + (field_y.x - field_y.y)^2 ) / ((timestamp.x - timestamp.y) /1000) )

run_max <- run_max %>%
    filter(!is.na(max_speed))

run_top_speed <- run_max %>%
    group_by(play_key) %>%
    summarise(top_speed = max(max_speed), .groups = "drop") %>%
    ungroup()

final_2nd_stats <- full_join(final_2nd_stats, run_top_speed, by="play_key") %>%
    relocate(top_speed, .after = run_speed)
final_2nd_stats <- final_2nd_stats %>% filter(run_dist > 0)
final_2nd_stats <- final_2nd_stats %>% filter(score_chance > -2)

lda_2nd_stats = lda(final_2nd_stats[, 4:9], grouping = final_2nd_stats$go_safe_out)

predict_2nd_stats = predict(lda_2nd_stats, final_2nd_stats[, 4:9])

sum(predict_2nd_stats$class == final_2nd_stats$go_safe_out)
[1] 536
536 of 593 correct

final_2nd_stats$lda_score_prob <- predict_2nd_stats$posterior[, "1"]
went_2nd_stats <- final_2nd_stats %>% filter(score_chance > -1)

lda_2nd_stats_2 = lda(went_2nd_stats[, 4:9], grouping = went_2nd_stats$safe_out)

predict_2nd_stats_2 = predict(lda_2nd_stats_2, went_2nd_stats[, 4:9])

sum(predict_2nd_stats_2$class == went_2nd_stats$safe_out)
[1] 391
391 of 410 correct, but didn’t predict any outs

went_2nd_stats$lda2_score_prob <- predict_2nd_stats_2$posterior[, "1"]
went_2nd_stats <- went_2nd_stats %>%
    mutate(all_bang_out = ifelse(score_chance < 1, 0, 1))

lda_2nd_stats_3 = lda(went_2nd_stats[, 4:9], grouping = went_2nd_stats$all_bang_out)

predict_2nd_stats_3 = predict(lda_2nd_stats_3, went_2nd_stats[, 4:9])

sum(predict_2nd_stats_3$class == went_2nd_stats$safe_out)
[1] 391
391 of 410 correct, stayed safe_out on purpose

went_2nd_stats$lda2_score_prob <- predict_2nd_stats_2$posterior[, "1"]
score_model <- glm(safe_out ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = went_2nd_stats, family = "binomial")

went_2nd_stats$model_score_prob <- predict(score_model, type = "response")

summary(score_model)

Call:
glm(formula = safe_out ~ run_dist + OF_dist + run_speed + top_speed + 
    OF_momentum_home + OF_momentum_side, family = "binomial", 
    data = went_2nd_stats)

Coefficients:
                  Estimate Std. Error z value Pr(>|z|)    
(Intercept)      -4.381585   7.243428  -0.605  0.54524    
run_dist         -0.148958   0.031694  -4.700  2.6e-06 ***
OF_dist           0.059891   0.018528   3.232  0.00123 ** 
run_speed        -0.034242   0.160512  -0.213  0.83107    
top_speed         0.268221   0.211555   1.268  0.20485    
OF_momentum_home -0.049608   0.058217  -0.852  0.39414    
OF_momentum_side -0.002717   0.055964  -0.049  0.96128    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 153.831  on 409  degrees of freedom
Residual deviance:  96.182  on 403  degrees of freedom
AIC: 110.18

Number of Fisher Scoring iterations: 9
lda_acc <- sum(abs(went_2nd_stats$lda_score_prob - went_2nd_stats$safe_out))

lda2_acc <- sum(abs(went_2nd_stats$lda2_score_prob - went_2nd_stats$safe_out))

lda3_acc <- sum(abs(went_2nd_stats$lda3_score_prob - went_2nd_stats$safe_out))

model_acc <- sum(abs(went_2nd_stats$model_score_prob - went_2nd_stats$safe_out))
install.packages("randomForest")
library(randomForest)

rand_forest <- randomForest(as.factor(safe_out) ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, 
                         data = went_2nd_stats, 
                         ntree = 500, 
                         importance = TRUE, 
                         probability = TRUE)

rand_for_prob <- predict(rand_forest, type = "prob")

went_2nd_stats$rand_forest_prob <- rand_for_prob[, "1"]

rand_forest_acc <- sum(abs(went_2nd_stats$rand_forest_prob - went_2nd_stats$safe_out))
rand_forest2 <- randomForest(as.factor(safe_out) ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, 
                         data = went_2nd_stats, 
                         ntree = 2000, 
                         importance = TRUE, 
                         probability = TRUE)

rand_for_prob2 <- predict(rand_forest2, type = "prob")

went_2nd_stats$rand_forest_prob2 <- rand_for_prob2[, "1"]

rand_forest2_acc <- sum(abs(went_2nd_stats$rand_forest_prob2 - went_2nd_stats$safe_out))
install.packages("xgboost")
library(xgboost)

xgb_model <- xgboost(
    data = as.matrix(went_2nd_stats[,4:9]),
    label = went_2nd_stats$safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    nrounds = 100,                   # number of boosting rounds
    max_depth = 3,                   # depth of trees
    eta = 0.1,                       # learning rate
    verbose = 0                      # turn off training printout
)

went_2nd_stats$xgb_prob <- predict(xgb_model, newdata = as.matrix(went_2nd_stats[,4:9]))

xgb_acc <- sum(abs(went_2nd_stats$xgb_prob - went_2nd_stats$safe_out))

final_2nd_stats$xgb_prob <- predict(xgb_model, newdata = as.matrix(final_2nd_stats[,4:9]))
final_2nd_stats$model_score_prob <- predict(score_model, newdata = final_2nd_stats, type = "response")
score_model_2 <- glm(go_safe_out ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = final_2nd_stats, family = "binomial")

final_2nd_stats$model_score_prob <- predict(score_model_2, type = "response")

summary(score_model_2)

Call:
glm(formula = go_safe_out ~ run_dist + OF_dist + run_speed + 
    top_speed + OF_momentum_home + OF_momentum_side, family = "binomial", 
    data = final_2nd_stats)

Coefficients:
                  Estimate Std. Error z value Pr(>|z|)    
(Intercept)      -14.45590    3.93943  -3.670 0.000243 ***
run_dist          -0.20373    0.02390  -8.525  < 2e-16 ***
OF_dist            0.07721    0.01240   6.228 4.72e-10 ***
run_speed          0.13824    0.07083   1.952 0.050971 .  
top_speed          0.46341    0.10988   4.217 2.47e-05 ***
OF_momentum_home  -0.05144    0.03659  -1.406 0.159758    
OF_momentum_side   0.03636    0.03636   1.000 0.317261    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 760.77  on 592  degrees of freedom
Residual deviance: 190.11  on 586  degrees of freedom
AIC: 204.11

Number of Fisher Scoring iterations: 8
xgb_model_2 <- xgboost(
    data = as.matrix(final_2nd_stats[,4:9]),
    label = final_2nd_stats$go_safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    nrounds = 100,                   # number of boosting rounds
    max_depth = 3,                   # depth of trees
    eta = 0.1,                       # learning rate
    verbose = 0                      # turn off training printout
)

final_2nd_stats$xgb_prob <- predict(xgb_model_2, newdata = as.matrix(final_2nd_stats[,4:9]))

xgb.plot.importance(xgb.importance(model = xgb_model_2))

xgb_model_3 <- xgboost(
    data = as.matrix(final_2nd_stats[,4:9]),
    label = final_2nd_stats$go_safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    nrounds = 100,                   # number of boosting rounds
    early_stopping_rounds = 10,
    max_depth = 2,                   # depth of trees
    eta = 0.1,                       # learning rate
    verbose = 0                      # turn off training printout
)

final_2nd_stats$xgb_prob_2 <- predict(xgb_model_3, newdata = as.matrix(final_2nd_stats[,4:9]))

xgb.plot.importance(xgb.importance(model = xgb_model_3))
final_xgb_cv <- xgb.cv(
    data = as.matrix(final_2nd_stats[,4:9]),        # matrix of predictors
    label = final_2nd_stats$go_safe_out,        # target variable (0/1)
    nrounds = 1000,             # max number of boosting rounds
    nfold = 5,                  # 5-fold cross-validation
    early_stopping_rounds = 10, # stop if no improvement in 10 rounds
    objective = "binary:logistic",
    eval_metric = "auc",        # or "logloss", "error", etc.
    verbose = 0
)

final_xgb_cv$best_iteration
[1] 21
xgb_model_3 <- xgboost(
    data = as.matrix(final_2nd_stats[,4:9]),
    label = final_2nd_stats$go_safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    nrounds = 200,                   # number of boosting rounds
    max_depth = 2,                   # depth of trees
    eta = 0.05,                       # learning rate
    verbose = 0                      # turn off training printout
)

final_2nd_stats$xgb_prob_2 <- predict(xgb_model_3, newdata = as.matrix(final_2nd_stats[,4:9]))
even_final2nd <- final_2nd_stats[seq(2, nrow(final_2nd_stats), by = 2), ]
even_final2nd <- even_final2nd[,1:16]

odd_final2nd <- final_2nd_stats[seq(1, nrow(final_2nd_stats), by = 2), ]
odd_final2nd <- odd_final2nd[,1:16]
lda_even = lda(even_final2nd[, 4:9], grouping = even_final2nd$go_safe_out)
lda_predict_odd = predict(lda_even, odd_final2nd[, 4:9])
odd_final2nd$lda_prob <- lda_predict_odd$posterior[, "1"]

even_model <- glm(go_safe_out ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = even_final2nd, family = "binomial")
odd_final2nd$model_prob <- predict(score_model, newdata = odd_final2nd, type = "response")

xgb_even <- xgboost(
    data = as.matrix(even_final2nd[,4:9]),
    label = even_final2nd$go_safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    nrounds = 100,                   # number of boosting rounds
    max_depth = 3,                   # depth of trees
    eta = 0.1,                       # learning rate
    verbose = 0                      # turn off training printout
 )
odd_final2nd$xgb_prob <- predict(xgb_even, newdata = as.matrix(odd_final2nd[,4:9]))

xgb_even_2 <- xgboost(
    data = as.matrix(even_final2nd[,4:9]),
    label = even_final2nd$go_safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    nrounds = 100,                   # number of boosting rounds
    max_depth = 2,                   # depth of trees
    eta = 0.1,                       # learning rate
    verbose = 0                      # turn off training printout
)
odd_final2nd$xgb_prob_2 <- predict(xgb_even_2, newdata = as.matrix(odd_final2nd[,4:9]))

install.packages("mgcv")
library(mgcv)
gam_model <- gam(go_safe_out ~ s(run_dist) + s(OF_dist) + s(run_speed) + s(top_speed) + s(OF_momentum_home) + s(OF_momentum_side), data = even_final2nd, family = binomial)
odd_final2nd$gam_prob <- predict(gam_model, newdata = odd_final2nd, type = "response")

xgb_even_3 <- xgboost(
    data = as.matrix(even_final2nd[,4:9]),
    label = even_final2nd$go_safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    early_stopping_rounds = 5,
    nrounds = 100,                   # number of boosting rounds
    max_depth = 2,                   # depth of trees
    eta = 0.05,                       # learning rate
    verbose = 0                      # turn off training printout
)
odd_final2nd$xgb_prob_3 <- predict(xgb_even_3, newdata = as.matrix(odd_final2nd[,4:9]))

xgb_even_4 <- xgboost(
    data = as.matrix(even_final2nd[,4:9]),
    label = even_final2nd$go_safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    early_stopping_rounds = 5,
    nrounds = 1000,                   # number of boosting rounds
    max_depth = 2,                   # depth of trees
    eta = 0.01,                       # learning rate
    verbose = 0,                      # turn off training printout
    subsample = 0.5,
    lambda = 10,
    alpha = 5
)
odd_final2nd$xgb_prob_4 <- predict(xgb_even_4, newdata = as.matrix(odd_final2nd[,4:9]))

> sum(abs(odd_final2nd$go_safe_out - odd_final2nd$lda_prob))
[1] 46.6541
> sum(abs(odd_final2nd$go_safe_out - odd_final2nd$model_prob))
[1] 41.23853
> sum(abs(odd_final2nd$go_safe_out - odd_final2nd$xgb_prob))
[1] 32.54013
> sum(abs(odd_final2nd$go_safe_out - odd_final2nd$xgb_prob_2))
[1] 35.19774
> sum(abs(odd_final2nd$go_safe_out - odd_final2nd$gam_prob))
[1] 29.47452
> sum(abs(odd_final2nd$go_safe_out - odd_final2nd$xgb_prob_3))
[1] 43.21555
> sum(abs(odd_final2nd$go_safe_out - odd_final2nd$xgb_prob_4))
[1] 57.90372

> log_loss(odd_final2nd$go_safe_out, odd_final2nd$lda_prob)
[1] 0.2494317
> log_loss(odd_final2nd$go_safe_out, odd_final2nd$model_prob)
[1] 0.2546873
> log_loss(odd_final2nd$go_safe_out, odd_final2nd$xgb_prob)
[1] 0.2133973
> log_loss(odd_final2nd$go_safe_out, odd_final2nd$xgb_prob_2)
[1] 0.2011464
> log_loss(odd_final2nd$go_safe_out, odd_final2nd$gam_prob)
[1] 0.2456809
> log_loss(odd_final2nd$go_safe_out, odd_final2nd$xgb_prob_3)
[1] 0.214753
> log_loss(odd_final2nd$go_safe_out, odd_final2nd$xgb_prob_4)
[1] 0.2660298

> sum(abs(odd_final2nd$go_safe_out - round(odd_final2nd$lda_prob)))
[1] 30
> sum(abs(odd_final2nd$go_safe_out - round(odd_final2nd$model_prob)))
[1] 33
> sum(abs(odd_final2nd$go_safe_out - round(odd_final2nd$xgb_prob)))
[1] 28
> sum(abs(odd_final2nd$go_safe_out - round(odd_final2nd$xgb_prob_2)))
[1] 26
> sum(abs(odd_final2nd$go_safe_out - round(odd_final2nd$gam_prob)))
[1] 25
> sum(abs(odd_final2nd$go_safe_out - round(odd_final2nd$xgb_prob_3)))
[1] 25
> sum(abs(odd_final2nd$go_safe_out - round(odd_final2nd$xgb_prob_4)))
[1] 32

> sum(abs( ifelse(is.na(odd_final2nd$safe_out - round(odd_final2nd$lda_prob)), 0, odd_final2nd$safe_out - round(odd_final2nd$lda_prob)) ))
[1] 14
> sum(abs( ifelse(is.na(odd_final2nd$safe_out - round(odd_final2nd$model_prob)), 0, odd_final2nd$safe_out - round(odd_final2nd$model_prob)) ))
[1] 9
> sum(abs( ifelse(is.na(odd_final2nd$safe_out - round(odd_final2nd$xgb_prob)), 0, odd_final2nd$safe_out - round(odd_final2nd$xgb_prob)) ))
[1] 16
> sum(abs( ifelse(is.na(odd_final2nd$safe_out - round(odd_final2nd$xgb_prob_2)), 0, odd_final2nd$safe_out - round(odd_final2nd$xgb_prob_2)) ))
[1] 12
> sum(abs( ifelse(is.na(odd_final2nd$safe_out - round(odd_final2nd$gam_prob)), 0, odd_final2nd$safe_out - round(odd_final2nd$gam_prob)) ))
[1] 15
> sum(abs( ifelse(is.na(odd_final2nd$safe_out - round(odd_final2nd$xgb_prob_3)), 0, odd_final2nd$safe_out - round(odd_final2nd$xgb_prob_3)) ))
[1] 11
> sum(abs( ifelse(is.na(odd_final2nd$safe_out - round(odd_final2nd$xgb_prob_4)), 0, odd_final2nd$safe_out - round(odd_final2nd$xgb_prob_4)) ))
[1] 14

> sum(abs( ifelse(is.na(odd_final2nd$safe_out - odd_final2nd$lda_prob), 0, odd_final2nd$safe_out - odd_final2nd$lda_prob) ))
[1] 24.58916
> sum(abs( ifelse(is.na(odd_final2nd$safe_out - odd_final2nd$model_prob), 0, odd_final2nd$safe_out - odd_final2nd$model_prob) ))
[1] 13.07285
> sum(abs( ifelse(is.na(odd_final2nd$safe_out - odd_final2nd$xgb_prob), 0, odd_final2nd$safe_out - odd_final2nd$xgb_prob) ))
[1] 18.86781
> sum(abs( ifelse(is.na(odd_final2nd$safe_out - odd_final2nd$xgb_prob_2), 0, odd_final2nd$safe_out - odd_final2nd$xgb_prob_2) ))
[1] 20.28995
> sum(abs( ifelse(is.na(odd_final2nd$safe_out - odd_final2nd$gam_prob), 0, odd_final2nd$safe_out - odd_final2nd$gam_prob) ))
[1] 18.33091
> sum(abs( ifelse(is.na(odd_final2nd$safe_out - odd_final2nd$xgb_prob_3), 0, odd_final2nd$safe_out - odd_final2nd$xgb_prob_3) ))
[1] 24.54739
> sum(abs( ifelse(is.na(odd_final2nd$safe_out - odd_final2nd$xgb_prob_4), 0, odd_final2nd$safe_out - odd_final2nd$xgb_prob_4) ))
[1] 33.2992

> sum(odd_final2nd$go_safe_out)
[1] 199
> sum(odd_final2nd$lda_prob)
[1] 208.9977
> sum(odd_final2nd$model_prob)
[1] 227.0008
> sum(odd_final2nd$xgb_prob)
[1] 205.0569
> sum(odd_final2nd$xgb_prob_2)
[1] 204.49
> sum(odd_final2nd$gam_prob)
[1] 203.2396
> sum(odd_final2nd$xgb_prob_3)
[1] 203.2773
> sum(odd_final2nd$xgb_prob_4)
[1] 200.4739
install.packages("mgcv")
library(mgcv)

all_gam_model <- gam(go_safe_out ~ s(run_dist) + s(OF_dist) + s(run_speed) + s(top_speed) + s(OF_momentum_home) + s(OF_momentum_side), data = final_2nd_stats, family = binomial)

final_2nd_stats$gam_prob <- predict(all_gam_model, type = "response")
xgb_scale_model <- xgboost(
    data = as.matrix(went_2nd_stats[,4:9]),
    label = went_2nd_stats$safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    scale_pos_weight = 391 / 19,
    nrounds = 100,                   # number of boosting rounds
    max_depth = 3,                   # depth of trees
    eta = 0.1,                       # learning rate
    verbose = 0                      # turn off training printout
)

went_2nd_stats$xgb_scale_prob <- predict(xgb_scale_model, newdata = as.matrix(went_2nd_stats[,4:9]))
xgb_model_4 <- xgboost(
    data = as.matrix(final_2nd_stats[,4:9]),
    label = final_2nd_stats$go_safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    nrounds = 100,                   # number of boosting rounds
    early_stopping_rounds = 5,
    max_depth = 3,                   # depth of trees
    eta = 0.05,                       # learning rate
    verbose = 0                      # turn off training printout
)

final_2nd_stats$xgb_prob_3 <- predict(xgb_model_4, newdata = as.matrix(final_2nd_stats[,4:9]))
xgb_scale_model_2 <- xgboost(
    data = as.matrix(went_2nd_stats[,4:9]),
    label = went_2nd_stats$safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    scale_pos_weight = 19 / 391,
    nrounds = 100,                   # number of boosting rounds
    max_depth = 3,                   # depth of trees
    eta = 0.1,                       # learning rate
    verbose = 0                      # turn off training printout
)

went_2nd_stats$xgb_scale_prob_2 <- predict(xgb_scale_model_2, newdata = as.matrix(went_2nd_stats[,4:9]))
went_gam_model <- gam(safe_out ~ s(run_dist) + s(OF_dist) + s(run_speed) + s(top_speed) + s(OF_momentum_home) + s(OF_momentum_side), data = went_2nd_stats, family = binomial)

went_2nd_stats$gam_prob <- predict(went_gam_model, type = "response")

final_2nd_stats$gam_prob <- predict(went_gam_model, newdata = final_2nd_stats, type = "response")
xgb_model_5 <- xgboost(
    data = as.matrix(went_2nd_stats[,4:9]),
    label = went_2nd_stats$safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    nrounds = 100,                   # number of boosting rounds
    early_stopping_rounds = 5,
    max_depth = 3,                   # depth of trees
    eta = 0.05,                       # learning rate
    verbose = 0                      # turn off training printout
)

went_2nd_stats$xgb_prob_2 <- predict(xgb_model_5, newdata = as.matrix(went_2nd_stats[,4:9]))

final_2nd_stats$went_xgb_prob_2 <- predict(xgb_model_5, newdata = as.matrix(final_2nd_stats[,4:9]))
Ordered by score_chance, ones where LDA score prob < 5% and where they went

clear_out_and_went_stats <- final_2nd_stats[,1:17] %>%
    filter(lda_score_prob <= 0.05 | score_chance >= 0)

clear_out_and_went_stats <- clear_out_and_went_stats[,1:16]
coaws_xgb <- xgboost(
    data = as.matrix(clear_out_and_went_stats[,4:9]),
    label = clear_out_and_went_stats$go_safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    nrounds = 100,                   # number of boosting rounds
    max_depth = 3,                   # depth of trees
    eta = 0.1,                       # learning rate
    verbose = 0                      # turn off training printout
)

clear_out_and_went_stats$xgb <- predict(coaws_xgb, newdata = as.matrix(clear_out_and_went_stats[,4:9]))
coaws_xgb_2 <- xgboost(
    data = as.matrix(clear_out_and_went_stats[,4:9]),
    label = clear_out_and_went_stats$go_safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    nrounds = 200,                   # number of boosting rounds
    early_stopping_rounds = 5,
    max_depth = 3,                   # depth of trees
    eta = 0.05,                       # learning rate
    verbose = 0                      # turn off training printout
)

clear_out_and_went_stats$xgb_2 <- predict(coaws_xgb_2, newdata = as.matrix(clear_out_and_went_stats[,4:9]))
coaws_gam <- gam(go_safe_out ~ s(run_dist) + s(OF_dist) + s(run_speed) + s(top_speed) + s(OF_momentum_home) + s(OF_momentum_side), data = clear_out_and_went_stats, family = binomial)

clear_out_and_went_stats$gam <- predict(coaws_gam, type = "response")

final_2nd_stats$coaws_gam <- predict(coaws_gam, newdata = final_2nd_stats[,4:9], type = "response")
coaws_model <- glm(go_safe_out ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = clear_out_and_went_stats, family = "binomial")

clear_out_and_went_stats$model <- predict(coaws_model, newdata = clear_out_and_went_stats, type = "response")

clear_out_and_went_stats <- clear_out_and_went_stats %>%
    mutate(gam_min_model = gam - model)

final_2nd_stats$coaws_model <- predict(coaws_model, newdata = final_2nd_stats, type = "response")
even_went_2nd <- went_2nd_stats[seq(2, nrow(went_2nd_stats), by = 2), ]
even_went_2nd <- even_went_2nd[,1:19]

odd_went_2nd <- went_2nd_stats[seq(1, nrow(went_2nd_stats), by = 2), ]
odd_went_2nd <- odd_went_2nd[,1:19]
log_loss <- function(actual, predicted_probs, eps = 1e-15) {
  predicted_probs <- pmin(pmax(predicted_probs, eps), 1 - eps)  # avoid log(0)
  -mean(actual * log(predicted_probs) + (1 - actual) * log(1 - predicted_probs))
}
lda_even_2 = lda(even_went_2nd[, 4:9], grouping = even_went_2nd$safe_out)
lda_predict_odd_2 = predict(lda_even_2, odd_went_2nd[, 4:9])
odd_went_2nd$lda_prob <- lda_predict_odd_2$posterior[, "1"]

even_model_2 <- glm(safe_out ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = even_went_2nd, family = "binomial")
odd_went_2nd$model_prob <- predict(even_model_2, newdata = odd_went_2nd, type = "response")

xgb_went_even <- xgboost(
    data = as.matrix(even_went_2nd[,4:9]),
    label = even_went_2nd$safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    nrounds = 100,                   # number of boosting rounds
    max_depth = 3,                   # depth of trees
    eta = 0.1,                       # learning rate
    verbose = 0                      # turn off training printout
 )
odd_went_2nd$xgb_prob <- predict(xgb_went_even, newdata = as.matrix(odd_went_2nd[,4:9]))

xgb_went_even_2 <- xgboost(
    data = as.matrix(even_went_2nd[,4:9]),
    label = even_went_2nd$safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    nrounds = 200,                   # number of boosting rounds
    max_depth = 2,                   # depth of trees
    eta = 0.05,                       # learning rate
    verbose = 0                      # turn off training printout
 )
odd_went_2nd$xgb_prob_2 <- predict(xgb_went_even_2, newdata = as.matrix(odd_went_2nd[,4:9]))

gam_even_model <- gam(safe_out ~ s(run_dist) + s(OF_dist) + s(run_speed) + s(top_speed) + s(OF_momentum_home) + s(OF_momentum_side), data = even_went_2nd, family = binomial)
odd_went_2nd$gam_prob <- predict(gam_even_model, newdata = odd_went_2nd, type = "response")

xgb_went_even_3 <- xgboost(
    data = as.matrix(even_went_2nd[,4:9]),
    label = even_went_2nd$safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    early_stopping_rounds = 5,
    subsample = 0.5,
    nrounds = 100,                   # number of boosting rounds
    max_depth = 3,                   # depth of trees
    eta = 0.1,                       # learning rate
    verbose = 0                      # turn off training printout
 )
odd_went_2nd$xgb_prob_3 <- predict(xgb_went_even_3, newdata = as.matrix(odd_went_2nd[,4:9]))

xgb_went_even_4 <- xgboost(
    data = as.matrix(even_went_2nd[,4:9]),
    label = even_went_2nd$safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    early_stopping_rounds = 5,
    nrounds = 1000,                   # number of boosting rounds
    max_depth = 3,                   # depth of trees
    eta = 0.01,                       # learning rate
    verbose = 0,                      # turn off training printout
    subsample = 0.5,
    lambda = 5,
    alpha = 3
)
odd_went_2nd$xgb_prob_4 <- predict(xgb_went_even_4, newdata = as.matrix(odd_went_2nd[,4:9]))

> sum(abs(odd_went_2nd$safe_out - odd_went_2nd$lda_prob))
[1] 15.45387
> sum(abs(odd_went_2nd$safe_out - odd_went_2nd$model_prob))
[1] 14.82052
> sum(abs(odd_went_2nd$safe_out - odd_went_2nd$xgb_prob))
[1] 13.81208
> sum(abs(odd_went_2nd$safe_out - odd_went_2nd$xgb_prob_2))
[1] 14.38578
> sum(abs(odd_went_2nd$safe_out - odd_went_2nd$gam_prob))
[1] 15.01308
> sum(abs(odd_went_2nd$safe_out - odd_went_2nd$xgb_prob_3))
[1] 14.53796
> sum(abs(odd_went_2nd$safe_out - odd_went_2nd$xgb_prob_4))
[1] 20.07858

> cor(odd_went_2nd$safe_out, odd_went_2nd$lda_prob,  method = "pearson")
[1] 0.3180532
> cor(odd_went_2nd$safe_out, odd_went_2nd$model_prob,  method = "pearson")
[1] 0.3329426
> cor(odd_went_2nd$safe_out, odd_went_2nd$xgb_prob,  method = "pearson")
[1] 0.2693209
> cor(odd_went_2nd$safe_out, odd_went_2nd$xgb_prob_2,  method = "pearson")
[1] 0.2411321
> cor(odd_went_2nd$safe_out, odd_went_2nd$gam_prob,  method = "pearson")
[1] 0.3283522
> cor(odd_went_2nd$safe_out, odd_went_2nd$xgb_prob_3,  method = "pearson")
[1] 0.2322231
> cor(odd_went_2nd$safe_out, odd_went_2nd$xgb_prob_4,  method = "pearson")
[1] 0.2264516

> log_loss(odd_went_2nd$safe_out, odd_went_2nd$lda_prob)
[1] 0.140828
> log_loss(odd_went_2nd$safe_out, odd_went_2nd$model_prob)
[1] 0.1287586
> log_loss(odd_went_2nd$safe_out, odd_went_2nd$xgb_prob)
[1] 0.1574091
> log_loss(odd_went_2nd$safe_out, odd_went_2nd$xgb_prob_2)
[1] 0.1577127
> log_loss(odd_went_2nd$safe_out, odd_went_2nd$gam_prob)
[1] 0.1288078
> log_loss(odd_went_2nd$safe_out, odd_went_2nd$xgb_prob_3)
[1] 0.152947
> log_loss(odd_went_2nd$safe_out, odd_went_2nd$xgb_prob_4)
[1] 0.160157

> sum(abs(odd_went_2nd$safe_out - round(odd_went_2nd$lda_prob)))
[1] 9
> sum(abs(odd_went_2nd$safe_out - round(odd_went_2nd$model_prob)))
[1] 9
> sum(abs(odd_went_2nd$safe_out - round(odd_went_2nd$xgb_prob)))
[1] 11
> sum(abs(odd_went_2nd$safe_out - round(odd_went_2nd$xgb_prob_2)))
[1] 11
> sum(abs(odd_went_2nd$safe_out - round(odd_went_2nd$gam_prob)))
[1] 9
> sum(abs(odd_went_2nd$safe_out - round(odd_went_2nd$xgb_prob_3)))
[1] 10
> sum(abs(odd_went_2nd$safe_out - round(odd_went_2nd$xgb_prob_4)))
[1] 9

> sum(odd_went_2nd$safe_out)
[1] 196
> sum(odd_went_2nd$lda_prob)
[1] 196.6185
> sum(odd_went_2nd$model_prob)
[1] 195.7042
> sum(odd_went_2nd$xgb_prob)
[1] 197.0553
> sum(odd_went_2nd$xgb_prob_2)
[1] 197.0255
> sum(odd_went_2nd$gam_prob)
[1] 195.4968
> sum(odd_went_2nd$xgb_prob_3)
[1] 197.1909
> sum(odd_went_2nd$xgb_prob_4)
[1] 191.7643
lda_odd = lda(odd_went_2nd[, 4:9], grouping = odd_went_2nd$safe_out)
lda_predict_even = predict(lda_odd, even_went_2nd[, 4:9])
even_went_2nd$lda_prob <- lda_predict_even$posterior[, "1"]

odd_model <- glm(safe_out ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = odd_went_2nd, family = "binomial")
even_went_2nd$model_prob <- predict(odd_model, newdata = even_went_2nd, type = "response")

xgb_went_odd <- xgboost(
    data = as.matrix(odd_went_2nd[,4:9]),
    label = odd_went_2nd$safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    nrounds = 100,                   # number of boosting rounds
    max_depth = 3,                   # depth of trees
    eta = 0.1,                       # learning rate
    verbose = 0                      # turn off training printout
 )
even_went_2nd$xgb_prob <- predict(xgb_went_odd, newdata = as.matrix(even_went_2nd[,4:9]))

xgb_went_odd_2 <- xgboost(
    data = as.matrix(odd_went_2nd[,4:9]),
    label = odd_went_2nd$safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    nrounds = 200,                   # number of boosting rounds
    max_depth = 2,                   # depth of trees
    eta = 0.05,                       # learning rate
    verbose = 0                      # turn off training printout
 )
even_went_2nd$xgb_prob_2 <- predict(xgb_went_odd_2, newdata = as.matrix(even_went_2nd[,4:9]))

gam_odd_model <- gam(safe_out ~ s(run_dist) + s(OF_dist) + s(run_speed) + s(top_speed) + s(OF_momentum_home) + s(OF_momentum_side), data = odd_went_2nd, family = binomial)
even_went_2nd$gam_prob <- predict(gam_odd_model, newdata = even_went_2nd, type = "response")

xgb_went_odd_3 <- xgboost(
    data = as.matrix(odd_went_2nd[,4:9]),
    label = odd_went_2nd$safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    early_stopping_rounds = 5,
    subsample = 0.5,
    nrounds = 100,                   # number of boosting rounds
    max_depth = 3,                   # depth of trees
    eta = 0.1,                       # learning rate
    verbose = 0                      # turn off training printout
 )
even_went_2nd$xgb_prob_3 <- predict(xgb_went_odd_3, newdata = as.matrix(even_went_2nd[,4:9]))

xgb_went_odd_4 <- xgboost(
    data = as.matrix(odd_went_2nd[,4:9]),
    label = odd_went_2nd$safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    early_stopping_rounds = 5,
    nrounds = 1000,                   # number of boosting rounds
    max_depth = 3,                   # depth of trees
    eta = 0.01,                       # learning rate
    verbose = 0,                      # turn off training printout
    subsample = 0.5,
    lambda = 5,
    alpha = 3
)
even_went_2nd$xgb_prob_4 <- predict(xgb_went_odd_4, newdata = as.matrix(even_went_2nd[,4:9]))

gam_odd_model_2 <- gam(safe_out ~ s(run_dist) + s(OF_dist) + s(run_speed) + s(top_speed) + s(OF_momentum_home) + s(OF_momentum_side) + ti(run_dist, OF_dist)+ ti(run_speed, top_speed) + ti(run_dist, top_speed) + ti(OF_dist, OF_momentum_home) + ti(OF_momentum_home, OF_momentum_side), data = odd_went_2nd, family = binomial)
even_went_2nd$gam_prob_2 <- predict(gam_odd_model_2, newdata = even_went_2nd, type = "response")

> sum(abs(even_went_2nd$safe_out - even_went_2nd$lda_prob))
[1] 16.02592
> sum(abs(even_went_2nd$safe_out - even_went_2nd$model_prob))
[1] 14.04048
> sum(abs(even_went_2nd$safe_out - even_went_2nd$xgb_prob))
[1] 12.9313
> sum(abs(even_went_2nd$safe_out - even_went_2nd$xgb_prob_2))
[1] 13.35772
> sum(abs(even_went_2nd$safe_out - even_went_2nd$gam_prob))
[1] 13.77319
> sum(abs(even_went_2nd$safe_out - even_went_2nd$xgb_prob_3))
[1] 13.39225
> sum(abs(even_went_2nd$safe_out - even_went_2nd$xgb_prob_4))
[1] 19.42663
> sum(abs(even_went_2nd$safe_out - even_went_2nd$gam_prob_2))
[1] 12.02399

> cor(even_went_2nd$safe_out, even_went_2nd$lda_prob,  method = "pearson")
[1] 0.3620207
> cor(even_went_2nd$safe_out, even_went_2nd$model_prob,  method = "pearson")
[1] 0.49688
> cor(even_went_2nd$safe_out, even_went_2nd$xgb_prob,  method = "pearson")
[1] 0.3749273
> cor(even_went_2nd$safe_out, even_went_2nd$xgb_prob_2,  method = "pearson")
[1] 0.407719
> cor(even_went_2nd$safe_out, even_went_2nd$gam_prob,  method = "pearson")
[1] 0.4412483
> cor(even_went_2nd$safe_out, even_went_2nd$xgb_prob_3,  method = "pearson")
[1] 0.4190448
> cor(even_went_2nd$safe_out, even_went_2nd$xgb_prob_4,  method = "pearson")
[1] 0.3734187
> cor(even_went_2nd$safe_out, even_went_2nd$gam_prob_2,  method = "pearson")
[1] 0.4948555

> log_loss(even_went_2nd$safe_out, even_went_2nd$lda_prob)
[1] 0.1501183
> log_loss(even_went_2nd$safe_out, even_went_2nd$model_prob)
[1] 0.1125983
> log_loss(even_went_2nd$safe_out, even_went_2nd$xgb_prob)
[1] 0.147064
> log_loss(even_went_2nd$safe_out, even_went_2nd$xgb_prob_2)
[1] 0.1393545
> log_loss(even_went_2nd$safe_out, even_went_2nd$gam_prob)
[1] 0.1423269
> log_loss(even_went_2nd$safe_out, even_went_2nd$xgb_prob_3)
[1] 0.1415989
> log_loss(even_went_2nd$safe_out, even_went_2nd$xgb_prob_4)
[1] 0.1622245
> log_loss(even_went_2nd$safe_out, even_went_2nd$gam_prob_2)
[1] 0.1317856

> sum(abs(even_went_2nd$safe_out - round(even_went_2nd$lda_prob)))
[1] 10
> sum(abs(even_went_2nd$safe_out - round(even_went_2nd$model_prob)))
[1] 10
> sum(abs(even_went_2nd$safe_out - round(even_went_2nd$xgb_prob)))
[1] 12
> sum(abs(even_went_2nd$safe_out - round(even_went_2nd$xgb_prob_2)))
[1] 11
> sum(abs(even_went_2nd$safe_out - round(even_went_2nd$gam_prob)))
[1] 11
> sum(abs(even_went_2nd$safe_out - round(even_went_2nd$xgb_prob_3)))
[1] 11
> sum(abs(even_went_2nd$safe_out - round(even_went_2nd$xgb_prob_4)))
[1] 10
> sum(abs(even_went_2nd$safe_out - round(even_went_2nd$gam_prob_2)))
[1] 10

> sum(even_went_2nd$safe_out)
[1] 195
> sum(even_went_2nd$lda_prob)
[1] 197.0364
> sum(even_went_2nd$model_prob)
[1] 195.4255
> sum(even_went_2nd$xgb_prob)
[1] 198.8611
> sum(even_went_2nd$xgb_prob_2)
[1] 198.2718
> sum(even_went_2nd$gam_prob)
[1] 196.4156
> sum(even_went_2nd$xgb_prob_3)
[1] 198.7982
> sum(even_went_2nd$xgb_prob_4)
[1] 193.3329
> sum(even_went_2nd$gam_prob_2)
[1] 198.6244
score_model <- glm(safe_out ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = went_2nd_stats, family = "binomial")

final_2nd_stats$went_model_prob <- predict(score_model, newdata = final_2nd_stats, type = "response")

> sum(final_2nd_stats$went_model_prob)
[1] 445.4098
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


Use went data for all possible model ideas tried, split data in half, use one half as training, other half as test
Test by how far percentages are from actual, check how many it got right or wrong (>50% = think is safe)

save.image("SMT_data.RData")
write.csv(stats, "stats.csv", row.names = FALSE)








Runner on first

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
Baserunner distance to home (know they start at 2nd base)
3rd base is ( 90/sqrt(2) , 90/sqrt(2) )              ( 63.6 , 63.6 )
2nd base is ( 0 , 90*sqrt(2) )			( 0 , 127.3 )
3rd base is ( -90/sqrt(2) , 90/sqrt(2) )		( -63.6 , 63.6 )
Home is ( 0 , 0 )
3rd to home is 90 feet
if( (x >= 0) & (y > 90/sqrt(2)) )	            dist to 2nd + 180
else if( (x < 0) & (y > 90/sqrt(2)) )	dist to 3rd + 90
else					ifelse ( x > 0 | y < 0, 0, dist to home)

messy_stats_1st <- messy_stats_1st %>%
    mutate(run_dist = ifelse( (run_x >= 0 & run_y > 90/sqrt(2)) , 
        (180 + sqrt( (run_x)^2 + (run_y - (90*sqrt(2)))^2 ) ) ,
        ifelse( (run_x < 0 & run_y > 90/sqrt(2)) , 
            (90 + sqrt( (run_x + (90/sqrt(2)))^2 + (run_y - (90/sqrt(2)))^2 ) ) , 
            ifelse( (run_x > 0 | run_y < 0), 0, sqrt(run_x^2 + run_y^2) ) ) 
    ) )

messy_stats_1st <- messy_stats_1st %>%
    mutate(OF_dist = sqrt(OF_x^2 + OF_y^2))
run1st_lag_pos <- player_pos %>%
    # Create composite key
    mutate(key_run = paste(game_str, play_id, timestamp + 500, sep = "_")) %>%
    filter(
        key_run %in% messy_stats_1st$key_run,
        player_position == 11
    ) %>%
    # Deselect some unnecessary columns
    dplyr::select(-c("Season", "HomeTeam", "AwayTeam", "Day")) %>%
    collect()

run1st_lag_pos_2.0 <- player_pos %>%
    # Create composite key
    mutate(key_run = paste(game_str, play_id, timestamp + 495, sep = "_")) %>%
    filter(
        key_run %in% messy_stats_1st$key_run,
        player_position == 11
    ) %>%
    # Deselect some unnecessary columns
    dplyr::select(-c("Season", "HomeTeam", "AwayTeam", "Day")) %>%
    collect()

run1st_lag_pos <- rbind(run1st_lag_pos, run1st_lag_pos_2.0)

run1st_lag_pos <- run1st_lag_pos %>% rename(lag_timestamp = timestamp)
run1st_lag_pos <- run1st_lag_pos %>% rename(lag_run_x = field_x)
run1st_lag_pos <- run1st_lag_pos %>% rename(lag_run_y = field_y)

messy_stats_1st <- full_join(messy_stats_1st , run1st_lag_pos[,c(3,5:7)], by = "key_run")
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

messy_stats_1st <- full_join(messy_stats_1st, run1st_OF_lag_pos[,5:7], by = "lag_key_of")
messy_stats_1st <- messy_stats_1st %>%
    mutate( run_speed = ( sqrt((run_x - lag_run_x)^2 + (run_y - lag_run_y)^2) /  ((timestamp - lag_timestamp)/1000) ) )

messy_stats_1st <- messy_stats_1st %>%
    mutate(
        OF_x_velo = ( (OF_x - lag_OF_x) / ((timestamp - lag_timestamp)/1000) ),
        OF_y_velo = ( (OF_y - lag_OF_y) / ((timestamp - lag_timestamp)/1000) )
    )
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
messy_stats_1st <- messy_stats_1st %>%
    mutate(
        OF_momentum_home = ifelse( OF_home_y >= 0,
            -sqrt(OF_home_x^2 + OF_home_y^2) ,
            sqrt(OF_home_x^2 + OF_home_y^2) ) ,
        OF_momentum_side = sqrt(OF_side_x^2 + OF_side_y^2)
    )
messy_stats_1st <- messy_stats_1st %>%
    mutate(play_key = paste(game_str, play_id, sep = "_"))

go_home_1st <- player_pos %>%
    # Create composite key
    mutate(play_key = paste(game_str, play_id, sep = "_")) %>%
    filter(
        play_key %in% messy_stats_1st$play_key,
        player_position == 11
    ) %>%
    # Deselect some unnecessary columns
    dplyr::select(-c("Season", "HomeTeam", "AwayTeam", "Day")) %>%
    collect()
go_home_1st <- go_home_1st %>%
    mutate(home_dist = sqrt(field_x^2 + field_y^2) )

go_home_1st <- go_home_1st %>%
    mutate(going = ifelse( home_dist <= 45, 1, 0) )
close_home_1st <- go_home_1st %>%
    group_by(play_key) %>%
    slice_min(order_by = home_dist, n = 1, with_ties = FALSE) %>%
    ungroup()

close_home_1st <- close_home_1st %>% rename(run_home_time = timestamp)

messy_stats_1st <- full_join(messy_stats_1st, close_home_1st[,c(3,7:9)], by = "play_key")
catcher_rec_1st <- game_events %>%
    # Create composite key
    mutate(play_key = paste(game_str, play_id, sep = "_")) %>%
    filter(
        play_key %in% messy_stats_1st$play_key ,
        player_position == 2 ,
        event_code == 2
    ) %>%
    # Deselect some unnecessary columns
    dplyr::select(-c("Season", "HomeTeam", "AwayTeam", "Day", "at_bat", "play_per_game")) %>%
    collect()

catcher_rec_1st <- catcher_rec_1st %>% rename(catch_time = timestamp)
messy_stats_1st <- full_join(messy_stats_1st, catcher_rec_1st[,c(3,6)], by = "play_key")

messy_stats_1st <- messy_stats_1st %>%
    mutate(run_min_catch_time = (run_home_time - catch_time) / 1000)

messy_stats_1st <- messy_stats_1st %>%
    mutate(catch_before_score = ifelse( run_min_catch_time > 0, 1, 0 ) )
stats_1st <- messy_stats_1st %>%
    dplyr::select(game_str, play_id, run_dist, OF_dist, run_speed, OF_momentum_home, OF_momentum_side, home_dist, going, run_min_catch_time, catch_before_score)
Make final_1st_stats checking if players are safe or out

write.csv(stats_1st, "stats_1st.csv", row.names = FALSE)
final_1st_stats <- read_csv("final_1st_stats.csv")

final_1st_stats <- final_1st_stats %>%
    filter(!is.na(score_chance))

final_1st_stats <- final_1st_stats %>%
    mutate( safe_out = ifelse(score_chance == -1, NA, ifelse(score_chance <= 0.5, 0, 1) ) )
final_1st_stats <- final_1st_stats %>%
    mutate(play_key = paste(game_str, play_id, sep = "_")) %>%
    relocate(play_key, .after = play_id)

run_max_1st <- player_pos %>%
    # Create composite key
    mutate(play_key = paste(game_str, play_id, sep = "_")) %>%
    filter(
        play_key %in% final_1st_stats$play_key,
        player_position == 11
    ) %>%
    collect()

run_max_1st <- run_max_1st %>%
    mutate(time_key = paste(play_key, timestamp, sep="_"))
lag_run_max1_1st <- run_max_1st[,1:11]

lag_run_max1_1st <- lag_run_max1_1st %>%
    mutate(time_key = paste(play_key, as.numeric(timestamp) + 500, sep="_"))

lag_run_max2_1st <- run_max_1st[,1:11]

lag_run_max2_1st <- lag_run_max2_1st %>%
    mutate(time_key = paste(play_key, as.numeric(timestamp) + 495, sep="_"))

lag_run_max_1st <- rbind(lag_run_max1_1st, lag_run_max2_1st)
run_max_1st <- full_join(run_max_1st, lag_run_max_1st[,c(3,5:6,12)], by="time_key")

run_max_1st <- run_max_1st %>%
    mutate(max_speed = sqrt( (field_x.x - field_x.y)^2 + (field_y.x - field_y.y)^2 ) / ((timestamp.x - timestamp.y) /1000) )

run_max_1st <- run_max_1st %>%
    filter(!is.na(max_speed))

run_top_speed_1st <- run_max_1st %>%
    group_by(play_key) %>%
    summarise(top_speed = max(max_speed), .groups = "drop") %>%
    ungroup()

final_1st_stats <- full_join(final_1st_stats, run_top_speed_1st, by="play_key") %>%
    relocate(top_speed, .after = run_speed)
final_1st_stats <- final_1st_stats %>%
    filter(!is.na(run_dist))

went_1st_stats <- final_1st_stats %>%
    filter(!is.na(safe_out))

final_1st_data <- final_1st_stats[,c(1:9,14:15)]

final_1st_data <- final_1st_data %>%
    mutate(go_safe_out = ifelse(is.na(safe_out), 0, safe_out))

went_1st_data <- final_1st_data %>%
    filter(!is.na(safe_out))

final_1st_data <- final_1st_data %>%
    filter(play_key != "y1_d006_JJS_YJD_163")

final_1st_data <- final_1st_data %>%
    mutate(scoring_from = 1)

final_2nd_data <- final_2nd_data %>%
    mutate(scoring_from = 2)

all_final_data <- rbind(final_1st_data, final_2nd_data)

ball_hit <- game_events %>%
    # Create composite key
    mutate(play_key = paste(game_str, play_id, sep = "_")) %>%
    filter(
        play_key %in% all_final_data$play_key,
        event_code == 4
    ) %>%
    collect()

all_final_data <- full_join(all_final_data, ball_hit[,c(7,12)], by = "play_key")

all_final_data <- all_final_data %>%
    filter(!is.na(event_code))

all_final_data <- all_final_data[,1:11]

all_went_data <- all_final_data %>%
    filter(!is.na(safe_out))
aw_even_data <- all_went_data[seq(2, nrow(all_went_data), by = 2), ]

aw_odd_data <- all_went_data[seq(1, nrow(all_went_data), by = 2), ]
aw_lda_even = lda(aw_even_data[, 2:7], grouping = aw_even_data$safe_out)
aw_lda_pred_odd = predict(aw_lda_even, aw_odd_data[, 2:7])
aw_odd_data$lda <- aw_lda_pred_odd$posterior[, "1"]

aw_even_glm <- glm(safe_out ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = aw_even_data, family = "binomial")
aw_odd_data$glm <- predict(aw_even_glm, newdata = aw_odd_data, type = "response")

aw_even_gam <- gam(safe_out ~ s(run_dist) + s(OF_dist) + s(run_speed) + s(top_speed) + s(OF_momentum_home) + s(OF_momentum_side), data = aw_even_data, family = binomial, method = "REML")
aw_odd_data$gam <- predict(aw_even_gam, newdata = aw_odd_data, type = "response")

aw_xgb_even <- xgboost(
    data = as.matrix(aw_even_data[, 2:7]),
    label = aw_even_data$safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    nrounds = 100,                   # number of boosting rounds
    max_depth = 3,                   # depth of trees
    eta = 0.1,                       # learning rate
    verbose = 0                      # turn off training printout
 )
aw_odd_data$xgb <- predict(aw_xgb_even, newdata = as.matrix(aw_odd_data[,2:7]))

aw_even_gam_2 <- gam(safe_out ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side + run_dist:OF_dist + run_dist:top_speed + run_speed:top_speed + OF_dist:OF_momentum_home + OF_momentum_home:OF_momentum_side, data = aw_even_data, family = binomial, method = "REML")
aw_odd_data$gam_2 <- predict(aw_even_gam_2, newdata = aw_odd_data, type = "response")

aw_even_gam_3 <- gam(safe_out ~ s(run_dist) + OF_dist + s(run_speed) + top_speed + OF_momentum_home + OF_momentum_side, data = aw_even_data, family = binomial, method = "REML")
aw_odd_data$gam_3 <- predict(aw_even_gam_3, newdata = aw_odd_data, type = "response")

> sum(aw_odd_data$safe_out)
[1] 261
> sum(aw_odd_data$lda)
[1] 271.973
> sum(aw_odd_data$glm)
[1] 269.8682
> sum(aw_odd_data$gam)
[1] 271.5833
> sum(aw_odd_data$xgb)
[1] 274.3046
> sum(aw_odd_data$gam_2)
[1] 269.2056
> sum(aw_odd_data$gam_3)
[1] 271.4794

Lower better
> log_loss(aw_odd_data$safe_out, aw_odd_data$lda)
[1] 0.223605
> log_loss(aw_odd_data$safe_out, aw_odd_data$glm)
[1] 0.2119385
> log_loss(aw_odd_data$safe_out, aw_odd_data$gam)
[1] 0.2444302
> log_loss(aw_odd_data$safe_out, aw_odd_data$xgb)
[1] 0.300047
> log_loss(aw_odd_data$safe_out, aw_odd_data$gam_2)
[1] 0.2193185
> log_loss(aw_odd_data$safe_out, aw_odd_data$gam_3)
[1] 0.2240399

install.packages("pROC")
library(pROC)
Higher better
> auc(roc(aw_odd_data$safe_out, aw_odd_data$lda))
Setting levels: control = 0, case = 1
Setting direction: controls < cases
Area under the curve: 0.8595
> auc(roc(aw_odd_data$safe_out, aw_odd_data$glm))
Setting levels: control = 0, case = 1
Setting direction: controls < cases
Area under the curve: 0.8776
> auc(roc(aw_odd_data$safe_out, aw_odd_data$gam))
Setting levels: control = 0, case = 1
Setting direction: controls < cases
Area under the curve: 0.8537
> auc(roc(aw_odd_data$safe_out, aw_odd_data$xgb))
Setting levels: control = 0, case = 1
Setting direction: controls < cases
Area under the curve: 0.7875
> auc(roc(aw_odd_data$safe_out, aw_odd_data$gam_2))
Setting levels: control = 0, case = 1
Setting direction: controls < cases
Area under the curve: 0.8686
> auc(roc(aw_odd_data$safe_out, aw_odd_data$gam_3))
Setting levels: control = 0, case = 1
Setting direction: controls < cases
Area under the curve: 0.8692
aw_lda_odd = lda(aw_odd_data[, 2:7], grouping = aw_odd_data$safe_out)
aw_lda_pred_even = predict(aw_lda_odd, aw_even_data[, 2:7])
aw_even_data$lda <- aw_lda_pred_even$posterior[, "1"]

aw_odd_glm <- glm(safe_out ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = aw_odd_data, family = "binomial")
aw_even_data$glm <- predict(aw_odd_glm, newdata = aw_even_data, type = "response")

aw_odd_gam <- gam(safe_out ~ s(run_dist) + s(OF_dist) + s(run_speed) + s(top_speed) + s(OF_momentum_home) + s(OF_momentum_side), data = aw_odd_data, family = binomial, method = "REML")
aw_even_data$gam <- predict(aw_odd_gam, newdata = aw_even_data, type = "response")

aw_xgb_odd <- xgboost(
    data = as.matrix(aw_odd_data[, 2:7]),
    label = aw_odd_data$safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    nrounds = 100,                   # number of boosting rounds
    max_depth = 3,                   # depth of trees
    eta = 0.1,                       # learning rate
    verbose = 0                      # turn off training printout
 )
aw_even_data$xgb <- predict(aw_xgb_odd, newdata = as.matrix(aw_even_data[,2:7]))

aw_odd_gam_2 <- gam(safe_out ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side + run_dist:OF_dist + run_dist:top_speed + run_speed:top_speed + OF_dist:OF_momentum_home + OF_momentum_home:OF_momentum_side, data = aw_odd_data, family = binomial, method = "REML")
aw_even_data$gam_2 <- predict(aw_odd_gam_2, newdata = aw_even_data, type = "response")

aw_odd_gam_3 <- gam(safe_out ~ s(run_dist) + OF_dist + s(run_speed) + top_speed + OF_momentum_home + OF_momentum_side, data = aw_odd_data, family = binomial, method = "REML")
aw_even_data$gam_3 <- predict(aw_odd_gam_3, newdata = aw_even_data, type = "response")

> sum(aw_even_data$safe_out)
[1] 269
> sum(aw_even_data$lda)
[1] 261.5995
> sum(aw_even_data$glm)
[1] 260.7076
> sum(aw_even_data$gam)
[1] 260.1815
> sum(aw_even_data$xgb)
[1] 262.7977
> sum(aw_even_data$gam_2)
[1] 259.6402
> sum(aw_even_data$gam_3)
[1] 260.1815

Lower better
> log_loss(aw_even_data$safe_out, aw_even_data$lda)
[1] 0.1411845
> log_loss(aw_even_data$safe_out, aw_even_data$glm)
[1] 0.1334242
> log_loss(aw_even_data$safe_out, aw_even_data$gam)
[1] 0.1288581
> log_loss(aw_even_data$safe_out, aw_even_data$xgb)
[1] 0.1277608
> log_loss(aw_even_data$safe_out, aw_even_data$gam_2)
[1] 0.1441622
> log_loss(aw_even_data$safe_out, aw_even_data$gam_3)
[1] 0.1288581

Higher better
> auc(roc(aw_even_data$safe_out, aw_even_data$lda))
Setting levels: control = 0, case = 1
Setting direction: controls < cases
Area under the curve: 0.9226
> auc(roc(aw_even_data$safe_out, aw_even_data$glm))
Setting levels: control = 0, case = 1
Setting direction: controls < cases
Area under the curve: 0.9346
> auc(roc(aw_even_data$safe_out, aw_even_data$gam))
Setting levels: control = 0, case = 1
Setting direction: controls < cases
Area under the curve: 0.9371
> auc(roc(aw_even_data$safe_out, aw_even_data$xgb))
Setting levels: control = 0, case = 1
Setting direction: controls < cases
Area under the curve: 0.9077
> auc(roc(aw_even_data$safe_out, aw_even_data$gam_2))
Setting levels: control = 0, case = 1
Setting direction: controls < cases
Area under the curve: 0.9374
> auc(roc(aw_even_data$safe_out, aw_even_data$gam_3))
Setting levels: control = 0, case = 1
Setting direction: controls < cases
Area under the curve: 0.9371
all_went_stats <- all_went_data

aw_glm <- glm(safe_out ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = all_went_stats, family = "binomial")

all_went_stats$glm <- predict(aw_glm, newdata = all_went_stats, type = "response")

> summary(aw_glm)

Call:
glm(formula = safe_out ~ run_dist + OF_dist + run_speed + top_speed + 
    OF_momentum_home + OF_momentum_side, family = "binomial", 
    data = all_went_stats)

Coefficients:
                   Estimate Std. Error z value Pr(>|z|)    
(Intercept)       5.3093686  4.3408609   1.223   0.2213    
run_dist         -0.1186556  0.0189173  -6.272 3.56e-10 ***
OF_dist           0.0374220  0.0088873   4.211 2.55e-05 ***
run_speed         0.0248344  0.1201738   0.207   0.8363    
top_speed        -0.0339492  0.1540169  -0.220   0.8255    
OF_momentum_home -0.0656133  0.0320168  -2.049   0.0404 *  
OF_momentum_side  0.0001532  0.0380831   0.004   0.9968    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 256.65  on 561  degrees of freedom
Residual deviance: 166.02  on 555  degrees of freedom
AIC: 180.02

Number of Fisher Scoring iterations: 8
aw_gam <- gam(safe_out ~ s(run_dist) + s(OF_dist) + s(run_speed) + s(top_speed) + s(OF_momentum_home) + s(OF_momentum_side), data = all_went_stats, family = binomial, method = "REML")

all_went_stats$gam <- predict(aw_gam, newdata = all_went_stats, type = "response")

plot(aw_gam, pages = 1)

aw_gam_2 <- gam(safe_out ~ s(run_dist) + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = all_went_stats, family = binomial, method = "REML")

all_went_stats$gam_2 <- predict(aw_gam_2, newdata = all_went_stats, type = "response")
> sum(all_went_stats$safe_out)
[1] 528
> sum(all_went_stats$glm)
[1] 528

all_final_stats <- all_final_data
all_final_stats$glm <- predict(aw_glm, newdata = all_final_stats, type = "response")
> sum(all_final_stats$glm)
[1] 677.2394
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
all_went_data_2 <- all_final_data_2 %>%
    filter(!is.na(safe_out))

all_final_data_3 <- all_final_data_2 %>%
    filter( ifelse( ((baserunners == "1__" & next_baserunners == "12_") |  (baserunners == "12_" & (next_baserunners == "12_" | next_baserunners == "123")) |  (baserunners == "1_3" & next_baserunners == "12_") | (baserunners == "123" & (next_baserunners == "12_" | next_baserunners == "123"))) & scoring_from == 1 , 1, 0 ) != 1 )

all_final_data_3 <- all_final_data_3 %>%
    filter(run_dist <= 180)
WHILE DOING BASERUNNER WORK IN EXCEL, DELETED
y1_d082_FBP_QEA_71 FROM 1
y2_d041_XLB_RZQ_156 FROM 1
y1_d020_JFU_YJD_11 FROM 1
y1_d021_JFU_YJD_10 FROM 1
y1_d021_JFU_YJD_129 FROM 1
y2_d034_KNB_YJD_179 FROM 1
y1_d097_MRJ_YJD_71 FROM 1
y2_d092_NYA_YJD_260 FROM 1
y1_d058_WZR_YJD_330 FROM 1
y1_d079_FBP_QEA_267 FROM 2
y2_d065_HKR_RZQ_107 FROM 2
game_info_2 <- game_info %>%
    mutate(ppg_key = paste(game_str, play_per_game, sep = "_"))

game_events_2 <- game_events %>%
    mutate(ppg_key = paste(game_str, play_per_game, sep = "_"))

out_info <- full_join(game_info_2[,c(1,5:6,24)], game_events_2[,c(2,12)], by = "ppg_key")

out_info <- out_info %>%
    distinct(ppg_key, .keep_all = TRUE)

out_info <- out_info %>%
    filter(!is.na(game_str))

write.csv(out_info, "out_info.csv", row.names = FALSE)
Reloaded with half_inning and out data

> sum(out_info$outs)
[1] 71803
Same as number of rows, so average of 1 out, good

out_info <- out_info %>%
    mutate(play_key = paste(game_str, play_id, sep = "_"))

all_final_data_4 <- left_join(all_final_data_3, out_info[,7:8], by = "play_key")

all_final_data_4 <- all_final_data_4 %>%
    mutate(outs = ifelse(is.na(outs), 1, outs))

> sum(all_final_data_4$outs)
[1] 1323
Average of about 1.235 outs, makes sense since runners on base likely later in inning
install.packages("baseballr")
library(baseballr)

schedule <- mlb_schedule(season = 2021,
    level_ids = c(11:14))

game_ids <- unique(schedule$game_pk)

daily_stats <- function(game_id){
    tryCatch(
      mlb_pbp(game_pk = game_id) # |> 
      # Data cleaning/manipulation
      ,
      error = function(cond){tibble()}
    )
  }

test_pbp <- map(head(game_ids, 3), ~{ daily_stats(.x)}) %>%
    bind_rows()

test_pbp_data <- test_pbp %>%
    dplyr::select(game_pk, atBatIndex, result.awayScore, result.homeScore, count.outs.start, about.inning, about.halfInning, matchup.postOnFirst.fullName, matchup.postOnSecond.fullName, matchup.postOnThird.fullName)

test_pbp_data <- test_pbp_data %>%
    mutate(inning_key = paste(game_pk, about.halfInning, about.inning, sep = "_"))

test_pbp_data_ordered <- test_pbp_data %>%
    arrange(inning_key, atBatIndex)

test_pbp_data_ordered <- test_pbp_data_ordered %>%
    distinct(inning_key, atBatIndex, .keep_all = TRUE)

test_pbp_data_ordered <- test_pbp_data_ordered %>%
    mutate(current_runs = ifelse(about.halfInning == "top", result.awayScore, result.homeScore))

test_pbp_data_ordered <- test_pbp_data_ordered %>%
    group_by(inning_key) %>%
    mutate(end_half_runs = max(current_runs))

test_pbp_data_ordered <- test_pbp_data_ordered %>%
    mutate(runs_yet_to_score = end_half_runs - current_runs)

test_pbp_data_ordered <- test_pbp_data_ordered %>%
    mutate(baserunners = paste( ifelse(!is.na(matchup.postOnFirst.fullName), 1, "_"), ifelse(!is.na(matchup.postOnSecond.fullName), 2, "_"), ifelse(!is.na(matchup.postOnThird.fullName), 3, "_") , sep = "") )

test_pbp_data_ordered <- test_pbp_data_ordered %>%
    mutate(outs = count.outs.start)

test_pbp_data_ordered <- test_pbp_data_ordered %>%
    mutate(situation = paste(outs, baserunners, sep = " "))

test_run_expectency <- data.frame(situation = c(
    "0 ___", "0 1__", "0 _2_", "0 __3", "0 12_", "0 1_3", "0 _23", "0 123",
    "1 ___", "1 1__", "1 _2_", "1 __3", "1 12_", "1 1_3", "1 _23", "1 123",
    "2 ___", "2 1__", "2 _2_", "2 __3", "2 12_", "2 1_3", "2 _23", "2 123"))

test_RE_avgs <- test_pbp_data_ordered %>%
    group_by(situation) %>%
    summarize(RE = mean(runs_yet_to_score))

test_run_expectency <- left_join(test_run_expectency, test_RE_avgs, by = "situation")
library(purrr)
library(progressr)

handlers(global = TRUE)
handlers("txtprogressbar")

with_progress({
    p <- progressor(along = game_ids)
    
    pbp <- map(game_ids, ~{
        p()
        daily_stats(.x)
    }) %>%
        bind_rows()
})
pbp_data <- pbp %>%
    dplyr::select(game_pk, atBatIndex, result.awayScore, result.homeScore, count.outs.start, about.inning, about.halfInning, matchup.postOnFirst.fullName, matchup.postOnSecond.fullName, matchup.postOnThird.fullName)

pbp_data <- pbp_data %>%
    mutate(inning_key = paste(game_pk, about.halfInning, about.inning, sep = "_"))

pbp_data_ordered <- pbp_data %>%
    arrange(inning_key, atBatIndex)

pbp_data_ordered <- pbp_data_ordered %>%
    mutate(current_runs = ifelse(about.halfInning == "top", result.awayScore, result.homeScore))

pbp_data_ordered <- pbp_data_ordered %>%
    group_by(inning_key) %>%
    mutate(end_half_runs = max(current_runs))

pbp_data_ordered <- pbp_data_ordered %>%
    mutate(runs_yet_to_score = end_half_runs - current_runs)

pbp_data_ordered <- pbp_data_ordered %>%
    distinct(inning_key, atBatIndex, .keep_all = TRUE)

pbp_data_ordered <- pbp_data_ordered %>%
    mutate(baserunners = paste( ifelse(!is.na(matchup.postOnFirst.fullName), 1, "_"), ifelse(!is.na(matchup.postOnSecond.fullName), 2, "_"), ifelse(!is.na(matchup.postOnThird.fullName), 3, "_") , sep = "") )

pbp_data_ordered <- pbp_data_ordered %>%
    filter(count.outs.start < 3)

pbp_data_ordered <- pbp_data_ordered %>%
    mutate(situation = paste(count.outs.start, baserunners, sep = " "))

run_expectancy <- data.frame(situation = c(
    "0 ___", "0 1__", "0 _2_", "0 __3", "0 12_", "0 1_3", "0 _23", "0 123",
    "1 ___", "1 1__", "1 _2_", "1 __3", "1 12_", "1 1_3", "1 _23", "1 123",
    "2 ___", "2 1__", "2 _2_", "2 __3", "2 12_", "2 1_3", "2 _23", "2 123",            
    "3 ___", "3 1__", "3 _2_", "3 __3", "3 12_", "3 1_3", "3 _23", "3 123"))

RE_avgs <- pbp_data_ordered %>%
    group_by(situation) %>%
    summarize(RE = mean(runs_yet_to_score))

run_expectancy <- left_join(run_expectancy, RE_avgs, by = "situation")

run_expectancy <- run_expectancy %>%
    mutate(run_exp = ifelse(is.na(RE), 0, RE))

run_expectancy <- run_expectancy[,c(1,3)]
all_final_data_4 <- all_final_data_4 %>%
    mutate(go_safe = paste(outs, go_runners, sep = " "))

all_final_data_4 <- all_final_data_4 %>%
    mutate(go_out = paste(outs+1, go_runners, sep = " "))

all_final_data_4 <- all_final_data_4 %>%
    mutate(stay = paste(outs, stay_runners, sep = " "))

all_final_data_4 <- left_join(all_final_data_4, run_expectancy, by = c("go_safe" = "situation"))

all_final_data_4$run_exp <- all_final_data_4$run_exp + 1

all_final_data_4 <- left_join(all_final_data_4, run_expectancy, by = c("go_out" = "situation"))

all_final_data_4 <- left_join(all_final_data_4, run_expectancy, by = c("stay" = "situation"))

all_final_data_4 <- all_final_data_4 %>% rename(go_safe_RE = run_exp.x)
all_final_data_4 <- all_final_data_4 %>% rename(go_out_RE = run_exp.y)
all_final_data_4 <- all_final_data_4 %>% rename(stay_RE = run_exp)

all_final_data_4 <- all_final_data_4 %>% 
    mutate(prob_to_go = (stay_RE - go_out_RE)/(go_safe_RE - go_out_RE))
all_went_data <- all_final_data_4[,1:9] %>%
    filter(score_chance >= 0)

all_went_safe_data <- all_went_data %>%
    filter(safe_out == 1)

all_went_out_data <- all_went_data %>%
    filter(safe_out == 0)

all_went_out_data <- all_went_out_data %>%
    mutate(group = rep(1:4, length.out = 34))

all_went_safe_data <- all_went_safe_data %>%
    mutate(group = rep(1:4, length.out = 527))
all_went_train_1 <- rbind(
    all_went_safe_data %>% filter(group != 4),
    all_went_out_data %>% filter(group != 1)
    )
all_went_test_1 <- rbind(
    all_went_safe_data %>% filter(group == 4),
    all_went_out_data %>% filter(group == 1)
)

all_went_train_2 <- rbind(
    all_went_safe_data %>% filter(group != 4),
    all_went_out_data %>% filter(group != 1)
)
all_went_test_2 <- rbind(
    all_went_safe_data %>% filter(group == 1),
    all_went_out_data %>% filter(group == 4)
)

all_went_train_3 <- rbind(
    all_went_safe_data %>% filter(group != 2),
    all_went_out_data %>% filter(group != 3)
)
all_went_test_3 <- rbind(
    all_went_safe_data %>% filter(group == 2),
    all_went_out_data %>% filter(group == 3)
)

all_went_train_4 <- rbind(
    all_went_safe_data %>% filter(group != 3),
    all_went_out_data %>% filter(group != 2)
)
all_went_test_4 <- rbind(
    all_went_safe_data %>% filter(group == 3),
    all_went_out_data %>% filter(group == 2)
)
glm_1 <- glm(safe_out ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = all_went_train_1, family = "binomial")
all_went_test_1$glm <- predict(glm_1, newdata = all_went_test_1, type = "response")

glm_2 <- glm(safe_out ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = all_went_train_2, family = "binomial")
all_went_test_2$glm <- predict(glm_2, newdata = all_went_test_2, type = "response")

glm_3 <- glm(safe_out ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = all_went_train_3, family = "binomial")
all_went_test_3$glm <- predict(glm_3, newdata = all_went_test_3, type = "response")

glm_4 <- glm(safe_out ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = all_went_train_4, family = "binomial")
all_went_test_4$glm <- predict(glm_4, newdata = all_went_test_4, type = "response")

log_loss(
    c(all_went_test_1$safe_out, all_went_test_2$safe_out, all_went_test_3$safe_out, all_went_test_4$safe_out) ,
    c(all_went_test_1$glm, all_went_test_2$glm, all_went_test_3$glm, all_went_test_4$glm)
)
[1] 0.168083

auc(roc(c(all_went_test_1$safe_out, all_went_test_2$safe_out, all_went_test_3$safe_out, all_went_test_4$safe_out), c(all_went_test_1$glm, all_went_test_2$glm, all_went_test_3$glm, all_went_test_4$glm)))
Area under the curve: 0.8945

sum(
    abs( c(all_went_test_1$safe_out, all_went_test_2$safe_out, all_went_test_3$safe_out, all_went_test_4$safe_out) -
             ifelse(c(all_went_test_1$glm, all_went_test_2$glm, all_went_test_3$glm, all_went_test_4$glm) >= 0.5, 1, 0) )
)
[1] 34
gam_1 <- gam(safe_out ~ s(run_dist) + s(OF_dist) + s(run_speed) + s(top_speed) + s(OF_momentum_home) + s(OF_momentum_side), data = all_went_train_1, family = binomial)
all_went_test_1$gam <- predict(gam_1, newdata = all_went_test_1, type = "response")

gam_2 <- gam(safe_out ~ s(run_dist) + s(OF_dist) + s(run_speed) + s(top_speed) + s(OF_momentum_home) + s(OF_momentum_side), data = all_went_train_2, family = binomial)
all_went_test_2$gam <- predict(gam_2, newdata = all_went_test_2, type = "response")

gam_3 <- gam(safe_out ~ s(run_dist) + s(OF_dist) + s(run_speed) + s(top_speed) + s(OF_momentum_home) + s(OF_momentum_side), data = all_went_train_3, family = binomial)
all_went_test_3$gam <- predict(gam_3, newdata = all_went_test_3, type = "response")

gam_4 <- gam(safe_out ~ s(run_dist) + s(OF_dist) + s(run_speed) + s(top_speed) + s(OF_momentum_home) + s(OF_momentum_side), data = all_went_train_4, family = binomial)
all_went_test_4$gam <- predict(gam_4, newdata = all_went_test_4, type = "response")

log_loss(
    c(all_went_test_1$safe_out, all_went_test_2$safe_out, all_went_test_3$safe_out, all_went_test_4$safe_out) ,
    c(all_went_test_1$gam, all_went_test_2$gam, all_went_test_3$gam, all_went_test_4$gam)
)
[1] 0.1826407

auc(roc(c(all_went_test_1$safe_out, all_went_test_2$safe_out, all_went_test_3$safe_out, all_went_test_4$safe_out), c(all_went_test_1$gam, all_went_test_2$gam, all_went_test_3$gam, all_went_test_4$gam)))
Area under the curve: 0.8807

sum(
    abs( c(all_went_test_1$safe_out, all_went_test_2$safe_out, all_went_test_3$safe_out, all_went_test_4$safe_out) -
             ifelse(c(all_went_test_1$gam, all_went_test_2$gam, all_went_test_3$gam, all_went_test_4$gam) >= 0.5, 1, 0) )
)
[1] 37
xgb_1 <- xgboost(
    data = as.matrix(all_went_train_1[,2:7]),
    label = all_went_train_1$safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    nrounds = 100,                   # number of boosting rounds
    max_depth = 2,                   # depth of trees
    eta = 0.1,                       # learning rate
    verbose = 0                      # turn off training printout
)
all_went_test_1$xgb <- predict(xgb_1, newdata = as.matrix(all_went_test_1[,2:7]))

xgb_2 <- xgboost(
    data = as.matrix(all_went_train_2[,2:7]),
    label = all_went_train_2$safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    nrounds = 100,                   # number of boosting rounds
    max_depth = 2,                   # depth of trees
    eta = 0.1,                       # learning rate
    verbose = 0                      # turn off training printout
)
all_went_test_2$xgb <- predict(xgb_2, newdata = as.matrix(all_went_test_2[,2:7]))

xgb_3 <- xgboost(
    data = as.matrix(all_went_train_3[,2:7]),
    label = all_went_train_3$safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    nrounds = 100,                   # number of boosting rounds
    max_depth = 2,                   # depth of trees
    eta = 0.1,                       # learning rate
    verbose = 0                      # turn off training printout
)
all_went_test_3$xgb <- predict(xgb_3, newdata = as.matrix(all_went_test_3[,2:7]))

xgb_4 <- xgboost(
    data = as.matrix(all_went_train_4[,2:7]),
    label = all_went_train_4$safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    nrounds = 100,                   # number of boosting rounds
    max_depth = 2,                   # depth of trees
    eta = 0.1,                       # learning rate
    verbose = 0                      # turn off training printout
)
all_went_test_4$xgb <- predict(xgb_4, newdata = as.matrix(all_went_test_4[,2:7]))

log_loss(
    c(all_went_test_1$safe_out, all_went_test_2$safe_out, all_went_test_3$safe_out, all_went_test_4$safe_out) ,
    c(all_went_test_1$xgb, all_went_test_2$xgb, all_went_test_3$xgb, all_went_test_4$xgb)
)
[1] 0.160022

auc(roc(c(all_went_test_1$safe_out, all_went_test_2$safe_out, all_went_test_3$safe_out, all_went_test_4$safe_out), c(all_went_test_1$xgb, all_went_test_2$xgb, all_went_test_3$xgb, all_went_test_4$xgb)))
Area under the curve: 0.8856

sum(
    abs( c(all_went_test_1$safe_out, all_went_test_2$safe_out, all_went_test_3$safe_out, all_went_test_4$safe_out) -
             ifelse(c(all_went_test_1$xgb, all_went_test_2$xgb, all_went_test_3$xgb, all_went_test_4$xgb) >= 0.5, 1, 0) )
)
[1] 32
lda_1 = lda(all_went_train_1[,2:7], grouping = all_went_train_1$safe_out)
all_went_test_1$lda <- predict(lda_1, all_went_test_1[, 2:7])$posterior[, "1"]

lda_2 = lda(all_went_train_2[,2:7], grouping = all_went_train_2$safe_out)
all_went_test_2$lda <- predict(lda_2, all_went_test_2[, 2:7])$posterior[, "1"]

lda_3 = lda(all_went_train_3[,2:7], grouping = all_went_train_3$safe_out)
all_went_test_3$lda <- predict(lda_3, all_went_test_3[, 2:7])$posterior[, "1"]

lda_4 = lda(all_went_train_4[,2:7], grouping = all_went_train_4$safe_out)
all_went_test_4$lda <- predict(lda_4, all_went_test_4[, 2:7])$posterior[, "1"]

log_loss(
    c(all_went_test_1$safe_out, all_went_test_2$safe_out, all_went_test_3$safe_out, all_went_test_4$safe_out) ,
    c(all_went_test_1$lda, all_went_test_2$lda, all_went_test_3$lda, all_went_test_4$lda)
)
[1] 0.1771057

auc(roc(c(all_went_test_1$safe_out, all_went_test_2$safe_out, all_went_test_3$safe_out, all_went_test_4$safe_out), c(all_went_test_1$lda, all_went_test_2$lda, all_went_test_3$lda, all_went_test_4$lda)))
Area under the curve: 0.8746

sum(
    abs( c(all_went_test_1$safe_out, all_went_test_2$safe_out, all_went_test_3$safe_out, all_went_test_4$safe_out) -
             ifelse(c(all_went_test_1$lda, all_went_test_2$lda, all_went_test_3$lda, all_went_test_4$lda) >= 0.5, 1, 0) )
)
[1] 34
gam.2_1 <- gam(safe_out ~ s(run_dist) + s(OF_dist) + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = all_went_train_1, family = binomial)
all_went_test_1$gam_2 <- predict(gam.2_1, newdata = all_went_test_1, type = "response")

gam.2_2 <- gam(safe_out ~ s(run_dist) + s(OF_dist) + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = all_went_train_2, family = binomial)
all_went_test_2$gam_2 <- predict(gam.2_2, newdata = all_went_test_2, type = "response")

gam.2_3 <- gam(safe_out ~ s(run_dist) + s(OF_dist) + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = all_went_train_3, family = binomial)
all_went_test_3$gam_2 <- predict(gam.2_3, newdata = all_went_test_3, type = "response")

gam.2_4 <- gam(safe_out ~ s(run_dist) + s(OF_dist) + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = all_went_train_4, family = binomial)
all_went_test_4$gam_2 <- predict(gam.2_4, newdata = all_went_test_4, type = "response")

log_loss(
    c(all_went_test_1$safe_out, all_went_test_2$safe_out, all_went_test_3$safe_out, all_went_test_4$safe_out) ,
    c(all_went_test_1$gam_2, all_went_test_2$gam_2, all_went_test_3$gam_2, all_went_test_4$gam_2)
)
[1] 0.1654217

auc(roc(c(all_went_test_1$safe_out, all_went_test_2$safe_out, all_went_test_3$safe_out, all_went_test_4$safe_out), c(all_went_test_1$gam_2, all_went_test_2$gam_2, all_went_test_3$gam_2, all_went_test_4$gam_2)))
Area under the curve: 0.901

sum(
    abs( c(all_went_test_1$safe_out, all_went_test_2$safe_out, all_went_test_3$safe_out, all_went_test_4$safe_out) -
             ifelse(c(all_went_test_1$gam_2, all_went_test_2$gam_2, all_went_test_3$gam_2, all_went_test_4$gam_2) >= 0.5, 1, 0) )
)
[1] 35
gam.3_1 <- gam(safe_out ~ s(run_dist) + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = all_went_train_1, family = binomial)
all_went_test_1$gam_3 <- predict(gam.3_1, newdata = all_went_test_1, type = "response")

gam.3_2 <- gam(safe_out ~ s(run_dist) + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = all_went_train_2, family = binomial)
all_went_test_2$gam_3 <- predict(gam.3_2, newdata = all_went_test_2, type = "response")

gam.3_3 <- gam(safe_out ~ s(run_dist) + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = all_went_train_3, family = binomial)
all_went_test_3$gam_3 <- predict(gam.3_3, newdata = all_went_test_3, type = "response")

gam.3_4 <- gam(safe_out ~ s(run_dist) + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = all_went_train_4, family = binomial)
all_went_test_4$gam_3 <- predict(gam.3_4, newdata = all_went_test_4, type = "response")

log_loss(
    c(all_went_test_1$safe_out, all_went_test_2$safe_out, all_went_test_3$safe_out, all_went_test_4$safe_out) ,
    c(all_went_test_1$gam_3, all_went_test_2$gam_3, all_went_test_3$gam_3, all_went_test_4$gam_3)
)
[1] 0.1654206

auc(roc(c(all_went_test_1$safe_out, all_went_test_2$safe_out, all_went_test_3$safe_out, all_went_test_4$safe_out), c(all_went_test_1$gam_3, all_went_test_2$gam_3, all_went_test_3$gam_3, all_went_test_4$gam_3)))
Area under the curve: 0.901

sum(
    abs( c(all_went_test_1$safe_out, all_went_test_2$safe_out, all_went_test_3$safe_out, all_went_test_4$safe_out) -
             ifelse(c(all_went_test_1$gam_3, all_went_test_2$gam_3, all_went_test_3$gam_3, all_went_test_4$gam_3) >= 0.5, 1, 0) )
)
[1] 35
gam.4_1 <- gam(safe_out ~ te(run_dist, OF_dist) + te(run_speed, top_speed) + te(OF_momentum_home, OF_momentum_side), data = all_went_train_1, family = binomial)
all_went_test_1$gam_4 <- predict(gam.4_1, newdata = all_went_test_1, type = "response")

gam.4_2 <- gam(safe_out ~ te(run_dist, OF_dist) + te(run_speed, top_speed) + te(OF_momentum_home, OF_momentum_side), data = all_went_train_2, family = binomial)
all_went_test_2$gam_4 <- predict(gam.4_2, newdata = all_went_test_2, type = "response")

gam.4_3 <- gam(safe_out ~ te(run_dist, OF_dist) + te(run_speed, top_speed) + te(OF_momentum_home, OF_momentum_side), data = all_went_train_3, family = binomial)
all_went_test_3$gam_4 <- predict(gam.4_3, newdata = all_went_test_3, type = "response")

gam.4_4 <- gam(safe_out ~ te(run_dist, OF_dist) + te(run_speed, top_speed) + te(OF_momentum_home, OF_momentum_side), data = all_went_train_4, family = binomial)
all_went_test_4$gam_4 <- predict(gam.4_4, newdata = all_went_test_4, type = "response")

log_loss(
    c(all_went_test_1$safe_out, all_went_test_2$safe_out, all_went_test_3$safe_out, all_went_test_4$safe_out) ,
    c(all_went_test_1$gam_4, all_went_test_2$gam_4, all_went_test_3$gam_4, all_went_test_4$gam_4)
)
[1] 0.1884009

auc(roc(c(all_went_test_1$safe_out, all_went_test_2$safe_out, all_went_test_3$safe_out, all_went_test_4$safe_out), c(all_went_test_1$gam_4, all_went_test_2$gam_4, all_went_test_3$gam_4, all_went_test_4$gam_4)))
Area under the curve: 0.8789

sum(
    abs( c(all_went_test_1$safe_out, all_went_test_2$safe_out, all_went_test_3$safe_out, all_went_test_4$safe_out) -
             ifelse(c(all_went_test_1$gam_4, all_went_test_2$gam_4, all_went_test_3$gam_4, all_went_test_4$gam_4) >= 0.5, 1, 0) )
)
[1] 36
xgb_params <- expand.grid(
  eta = c(0.01, 0.05, 0.1, 0.15, 0.2),
  nrounds = c(20, 50, 100, 200, 500)
)

results <- data.frame()

for (i in 1:nrow(xgb_params)) {

  strat_folds <- createFolds(all_went_data$safe_out, k = 5, list = TRUE, returnTrain = FALSE)

  params <- list(
    objective = "binary:logistic",
    eval_metric = "logloss",
    eta = xgb_params$eta[i]
  )
  
  cv <- xgb.cv(
    params = params,
    data = as.matrix(all_went_data[,2:7]),
    label = all_went_data$safe_out,
    nrounds = xgb_params$nrounds[i],
    folds = strat_folds,
    verbose = 0
  )
  
  min_logloss <- min(cv$evaluation_log$test_logloss_mean)
  results <- rbind(results, cbind(xgb_params[i, ], logloss = min_logloss))
}

AFTER FIRST ROUND
eta_nrounds_res <- results

AFTER SECOND+ ROUND
eta_nrounds_res <- left_join(eta_nrounds_res, results, by = c("eta", "nrounds"))

eta_nrounds_res <- eta_nrounds_res %>%
    mutate(avg = rowMeans(eta_nrounds_res[, 3:7]) )

library(ggplot2)
ggplot(eta_nrounds_res %>% filter (avg < 0.22), aes(x = nrounds, y = eta, color = avg)) +
  geom_point() +                # Add points
  scale_color_gradient(low = "green", high = "red")


eta = 0.15, nrounds = 500


strat_folds <- createFolds(all_went_data$safe_out, k = 5, list = TRUE, returnTrain = FALSE)

xgb_params <- expand.grid(
  eta = c(0.01, 0.05, 0.1, 0.15, 0.2),
  nrounds = c(20, 50, 100, 200, 500)
)

results <- data.frame()

for (i in 1:nrow(xgb_params)) {

  params <- list(
    objective = "binary:logistic",
    eval_metric = "logloss",
    eta = xgb_params$eta[i]
  )
  
  cv <- xgb.cv(
    params = params,
    data = as.matrix(all_went_data[,2:7]),
    label = all_went_data$safe_out,
    nrounds = xgb_params$nrounds[i],
    folds = strat_folds,
    verbose = 0
  )
  
  min_logloss <- min(cv$evaluation_log$test_logloss_mean)
  results <- rbind(results, cbind(xgb_params[i, ], logloss = min_logloss))
}

0.01 early stops after 500, all others before, high eta like 0.15 stops before 50
xgb_params <- expand.grid(
  eta = c(0.01, 0.05, 0.1, 0.15, 0.2),
  early_stopping_rounds = c(1, 5, 10, 15, 20)
)

results <- data.frame()

for (i in 1:nrow(xgb_params)) {

  strat_folds <- createFolds(all_went_data$safe_out, k = 5, list = TRUE, returnTrain = FALSE)

  params <- list(
    objective = "binary:logistic",
    eval_metric = "logloss",
    eta = xgb_params$eta[i],
    early_stopping_rounds = xgb_params$early_stopping_rounds[i]
  )
  
  cv <- xgb.cv(
    params = params,
    data = as.matrix(all_went_data[,2:7]),
    label = all_went_data$safe_out,
    nrounds = 1000,
    folds = strat_folds,
    verbose = 0
  )
  
  min_logloss <- min(cv$evaluation_log$test_logloss_mean)
  results <- rbind(results, cbind(xgb_params[i, ], logloss = min_logloss))
}

AFTER FIRST ROUND
eta_esr_res <- results

AFTER SECOND+ ROUND
eta_esr_res <- left_join(eta_esr_res, results, by = c("eta", "early_stopping_rounds"))

eta_esr_res <- eta_esr_res %>%
    mutate(avg = rowMeans(eta_esr_res[, 3:7]) )

ggplot(eta_esr_res , aes(x = eta, y = early_stopping_rounds, color = avg)) +
  geom_point() +                # Add points
  scale_color_gradient(low = "green", high = "red")

xgb_params <- expand.grid(
  max_depth = c(1, 2, 3, 4),
  min_child_weight = c(1, 2, 3, 4, 5)
)

results <- data.frame()

for (i in 1:nrow(xgb_params)) {

  strat_folds <- createFolds(all_went_data$safe_out, k = 5, list = TRUE, returnTrain = FALSE)

  params <- list(
    objective = "binary:logistic",
    eval_metric = "logloss",
    max_depth = xgb_params$max_depth[i],
    min_child_weight = xgb_params$min_child_weight[i]
  )
  
  cv <- xgb.cv(
    params = params,
    data = as.matrix(all_went_data[,2:7]),
    label = all_went_data$safe_out,
    nrounds = 1000,
    eta = 0.15,
    early_stopping_rounds = 10,
    folds = strat_folds,
    verbose = 0
  )
  
  min_logloss <- min(cv$evaluation_log$test_logloss_mean)
  results <- rbind(results, cbind(xgb_params[i, ], logloss = min_logloss))
}

AFTER FIRST ROUND
md_mcw_res <- results

AFTER SECOND+ ROUND
md_mcw_res <- left_join(md_mcw_res, results, by = c("max_depth", "min_child_weight"))

md_mcw_res <- md_mcw_res %>%
    mutate(avg = rowMeans(md_mcw_res[, 3:7]) )

ggplot(md_mcw_res, aes(x = max_depth, y = min_child_weight, color = avg)) +
  geom_point() +                # Add points
  scale_color_gradient(low = "green", high = "red")

max_depth = 1, min_child_weight = 2.5
xgb_params <- expand.grid(
  nrounds = c(100, 200, 350, 500, 1000),
  gamma = c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5)
)

results <- data.frame()

for (i in 1:nrow(xgb_params)) {

  strat_folds <- createFolds(all_went_data$safe_out, k = 5, list = TRUE, returnTrain = FALSE)

  params <- list(
    objective = "binary:logistic",
    eval_metric = "logloss",
    gamma = xgb_params$gamma[i]
  )
  
  cv <- xgb.cv(
    params = params,
    data = as.matrix(all_went_data[,2:7]),
    label = all_went_data$safe_out,
    nrounds = xgb_params$nrounds[i],
    eta = 0.15,
    early_stopping_rounds = 10,
    max_depth = 1,
    min_child_weight = 2.5,
    folds = strat_folds,
    verbose = 0
  )
  
  min_logloss <- min(cv$evaluation_log$test_logloss_mean)
  results <- rbind(results, cbind(xgb_params[i, ], logloss = min_logloss))
}

AFTER FIRST ROUND
rounds_gamma_res <- results

AFTER SECOND+ ROUND
rounds_gamma_res <- left_join(rounds_gamma_res, results, by = c("nrounds", "gamma"))

rounds_gamma_res <- rounds_gamma_res %>%
    mutate(avg = rowMeans(rounds_gamma_res[, 3:7]) )

ggplot(rounds_gamma_res, aes(x = nrounds, y = gamma, color = avg)) +
  geom_point() +                # Add points
  scale_color_gradient(low = "green", high = "red")

0.75 gamma and 1000 rounds
xgb_params <- expand.grid(
  subsample = c(0.5, 0.6, 0.7, 0.8, 0.9, 1),
  colsample_bytree = c(0.5, 0.6, 0.7, 0.8, 0.9, 1)
)

results <- data.frame()

for (i in 1:nrow(xgb_params)) {

  strat_folds <- createFolds(all_went_data$safe_out, k = 5, list = TRUE, returnTrain = FALSE)

  params <- list(
    objective = "binary:logistic",
    eval_metric = "logloss",
    subsample = xgb_params$subsample[i],
    colsample_bytree = xgb_params$colsample_bytree[i]
  )
  
  cv <- xgb.cv(
    params = params,
    data = as.matrix(all_went_data[,2:7]),
    label = all_went_data$safe_out,
    nrounds = 1000,
    eta = 0.15,
    early_stopping_rounds = 10,
    max_depth = 1,
    min_child_weight = 2.5,
    gamma = 0.75,
    folds = strat_folds,
    verbose = 0
  )
  
  min_logloss <- min(cv$evaluation_log$test_logloss_mean)
  results <- rbind(results, cbind(xgb_params[i, ], logloss = min_logloss))
}

AFTER FIRST ROUND
subsample_res <- results

AFTER SECOND+ ROUND
subsample_res <- left_join(subsample_res, results, by = c("subsample", "colsample_bytree"))

subsample_res <- subsample_res %>%
    mutate(avg = rowMeans(subsample_res[, 3:7]) )

ggplot(subsample_res, aes(x = subsample, y = colsample_bytree, color = avg)) +
  geom_point() +                # Add points
  scale_color_gradient(low = "green", high = "red")

subsample = 0.9
xgb_params <- expand.grid(
  lambda = c(0, 1, 2, 3, 4, 5),
  max_delta_step = c(0, 2, 4, 6, 8, 10)
)

results <- data.frame()

for (i in 1:nrow(xgb_params)) {

  strat_folds <- createFolds(all_went_data$safe_out, k = 5, list = TRUE, returnTrain = FALSE)

  params <- list(
    objective = "binary:logistic",
    eval_metric = "logloss",
    lambda = xgb_params$lambda[i],
    max_delta_step = xgb_params$max_delta_step[i]
  )
  
  cv <- xgb.cv(
    params = params,
    data = as.matrix(all_went_data[,2:7]),
    label = all_went_data$safe_out,
    nrounds = 1000,
    eta = 0.15,
    early_stopping_rounds = 10,
    max_depth = 1,
    min_child_weight = 2.5,
    gamma = 0.75,
    subsample = 0.9,
    folds = strat_folds,
    verbose = 0
  )
  
  min_logloss <- min(cv$evaluation_log$test_logloss_mean)
  results <- rbind(results, cbind(xgb_params[i, ], logloss = min_logloss))
}

AFTER FIRST ROUND
lambda_mds_res <- results

AFTER SECOND+ ROUND
lambda_mds_res <- left_join(lambda_mds_res, results, by = c("lambda", "max_delta_step"))

lambda_mds_res <- lambda_mds_res %>%
    mutate(avg = rowMeans(lambda_mds_res [, 3:7]) )

ggplot(lambda_mds_res, aes(x = lambda, y = max_delta_step, color = avg)) +
  geom_point() +                # Add points
  scale_color_gradient(low = "green", high = "red")

Keep default
xgb.2_1 <- xgboost(
    data = as.matrix(all_went_train_1[,2:7]),
    label = all_went_train_1$safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    nrounds = 100,                   # number of boosting rounds
    max_depth = 2,                   # depth of trees
    eta = 0.1,                       # learning rate
    subsample = 0.8,
    verbose = 0                      # turn off training printout
)
all_went_test_1$xgb_2 <- predict(xgb.2_1, newdata = as.matrix(all_went_test_1[,2:7]))

xgb.2_2 <- xgboost(
    data = as.matrix(all_went_train_2[,2:7]),
    label = all_went_train_2$safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    nrounds = 100,                   # number of boosting rounds
    max_depth = 2,                   # depth of trees
    eta = 0.1,                       # learning rate
    subsample = 0.8,
    verbose = 0                      # turn off training printout
)
all_went_test_2$xgb_2 <- predict(xgb.2_2, newdata = as.matrix(all_went_test_2[,2:7]))

xgb.2_3 <- xgboost(
    data = as.matrix(all_went_train_3[,2:7]),
    label = all_went_train_3$safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    nrounds = 100,                   # number of boosting rounds
    max_depth = 2,                   # depth of trees
    eta = 0.1,                       # learning rate
    subsample = 0.8,
    verbose = 0                      # turn off training printout
)
all_went_test_3$xgb_2 <- predict(xgb.2_3, newdata = as.matrix(all_went_test_3[,2:7]))

xgb.2_4 <- xgboost(
    data = as.matrix(all_went_train_4[,2:7]),
    label = all_went_train_4$safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    nrounds = 100,                   # number of boosting rounds
    max_depth = 2,                   # depth of trees
    eta = 0.1,                       # learning rate
    subsample = 0.8,
    verbose = 0                      # turn off training printout
)
all_went_test_4$xgb_2 <- predict(xgb.2_4, newdata = as.matrix(all_went_test_4[,2:7]))

log_loss(
    c(all_went_test_1$safe_out, all_went_test_2$safe_out, all_went_test_3$safe_out, all_went_test_4$safe_out) ,
    c(all_went_test_1$xgb_2, all_went_test_2$xgb_2, all_went_test_3$xgb_2, all_went_test_4$xgb_2)
)
[1] 0.1624125

auc(roc(c(all_went_test_1$safe_out, all_went_test_2$safe_out, all_went_test_3$safe_out, all_went_test_4$safe_out), c(all_went_test_1$xgb_2, all_went_test_2$xgb_2, all_went_test_3$xgb_2, all_went_test_4$xgb_2)))
Area under the curve: 0.8811

sum(
    abs( c(all_went_test_1$safe_out, all_went_test_2$safe_out, all_went_test_3$safe_out, all_went_test_4$safe_out) -
             ifelse(c(all_went_test_1$xgb_2, all_went_test_2$xgb_2, all_went_test_3$xgb_2, all_went_test_4$xgb_2) >= 0.5, 1, 0) )
)
[1] 33
strat_folds <- createFolds(all_went_data$safe_out, k = 5, list = TRUE, returnTrain = FALSE)

bayesian_function <- function(eta, max_depth, subsample, gamma, min_child_weight, early_stopping_rounds) {
  
  params <- list(
    objective = "binary:logistic",
    eval_metric = "logloss",
    eta = 0.1 + (eta * (0.5 - 0.1)),
    max_depth = as.integer(max_depth),
    subsample = subsample,
    gamma = gamma,
    min_child_weight = 1 + (min_child_weight * (10 - 1))
  )

  cv <- xgb.cv(
    params = params,
    data = as.matrix(all_went_data[,2:7]),
    label = all_went_data$safe_out,
    nrounds = 100,
    early_stopping_rounds = as.integer(5 + (early_stopping_rounds * (20 - 5))),
    folds = strat_folds,
    verbose = 0
  )

  best_logloss <- min(cv$evaluation_log$test_logloss_mean)
  
  return(list(Score = -best_logloss))
}

bounds <- list(
  eta = c(0, 1),
  max_depth = c(1L, 4L),
  subsample = c(0.5, 1),
  gamma = c(0, 2),
  min_child_weight = c(0, 1),
  early_stopping_rounds = c(0, 1)
)

optObj <- bayesOpt(
  FUN = bayesian_function,
  bounds = bounds,
  initPoints = 10,
  iters.n = 20, 
  acq = "ucb",  
  verbose = 0
)

getBestPars(optObj)
print(-max(optObj$scoreSummary$Score))

$eta
[1] 0.5

$max_depth
[1] 2

$subsample
[1] 0.6

$gamma
[1] 0.4818268

$min_child_weight
[1] 5.405685

$early_stopping_rounds
[1] 6

[1] 0.1798154
$eta
[1] 0.2422121

$max_depth
[1] 1

$subsample
[1] 0.6914888

$gamma
[1] 0.7646761

$min_child_weight
[1] 1

$early_stopping_rounds
[1] 17

[1] 0.1858099





xgb.3_1 <- xgboost(
    data = as.matrix(all_went_train_1[,2:7]),
    label = all_went_train_1$safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    nrounds = 100,                   # number of boosting rounds
    max_depth = 3,                   # depth of trees
    eta = 0.2,                       # learning rate
    gamma = 0,
    min_child_weight = 1,
    early_stopping_rounds = 15,
    verbose = 0                      # turn off training printout
)
all_went_test_1$xgb_3 <- predict(xgb.3_1, newdata = as.matrix(all_went_test_1[,2:7]))

xgb.3_2 <- xgboost(
    data = as.matrix(all_went_train_2[,2:7]),
    label = all_went_train_2$safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    nrounds = 100,                   # number of boosting rounds
    max_depth = 3,                   # depth of trees
    eta = 0.2,                       # learning rate
    gamma = 0,
    min_child_weight = 1,
    early_stopping_rounds = 15,
    verbose = 0                      # turn off training printout
)
all_went_test_2$xgb_3 <- predict(xgb.3_2, newdata = as.matrix(all_went_test_2[,2:7]))

xgb.3_3 <- xgboost(
    data = as.matrix(all_went_train_3[,2:7]),
    label = all_went_train_3$safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    nrounds = 100,                   # number of boosting rounds
    max_depth = 3,                   # depth of trees
    eta = 0.2,                       # learning rate
    gamma = 0,
    min_child_weight = 1,
    early_stopping_rounds = 15,
    verbose = 0                      # turn off training printout
)
all_went_test_3$xgb_3 <- predict(xgb.3_3, newdata = as.matrix(all_went_test_3[,2:7]))

xgb.3_4 <- xgboost(
    data = as.matrix(all_went_train_4[,2:7]),
    label = all_went_train_4$safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    nrounds = 100,                   # number of boosting rounds
    max_depth = 3,                   # depth of trees
    eta = 0.2,                       # learning rate
    gamma = 0,
    min_child_weight = 1,
    early_stopping_rounds = 15,
    verbose = 0                      # turn off training printout
)
all_went_test_4$xgb_3 <- predict(xgb.3_4, newdata = as.matrix(all_went_test_4[,2:7]))

log_loss(
    c(all_went_test_1$safe_out, all_went_test_2$safe_out, all_went_test_3$safe_out, all_went_test_4$safe_out) ,
    c(all_went_test_1$xgb_3, all_went_test_2$xgb_3, all_went_test_3$xgb_3, all_went_test_4$xgb_3)
)
[1] 0.1854547
xgb.4_1 <- xgboost(
    data = as.matrix(all_went_train_1[,2:7]),
    label = all_went_train_1$safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    nrounds = 1000,
    eta = 0.15,
    early_stopping_rounds = 10,
    max_depth = 1,
    min_child_weight = 2.5,
    gamma = 0.75,
    subsample = 0.9,
    verbose = 0                      # turn off training printout
)
all_went_test_1$xgb_4 <- predict(xgb.4_1, newdata = as.matrix(all_went_test_1[,2:7]))

xgb.4_2 <- xgboost(
    data = as.matrix(all_went_train_2[,2:7]),
    label = all_went_train_2$safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    nrounds = 1000,
    eta = 0.15,
    early_stopping_rounds = 10,
    max_depth = 1,
    min_child_weight = 2.5,
    gamma = 0.75,
    subsample = 0.9,
    verbose = 0                      # turn off training printout
)
all_went_test_2$xgb_4 <- predict(xgb.4_2, newdata = as.matrix(all_went_test_2[,2:7]))

xgb.4_3 <- xgboost(
    data = as.matrix(all_went_train_3[,2:7]),
    label = all_went_train_3$safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    nrounds = 1000,
    eta = 0.15,
    early_stopping_rounds = 10,
    max_depth = 1,
    min_child_weight = 2.5,
    gamma = 0.75,
    subsample = 0.9,
    verbose = 0                      # turn off training printout
)
all_went_test_3$xgb_4 <- predict(xgb.4_3, newdata = as.matrix(all_went_test_3[,2:7]))

xgb.4_4 <- xgboost(
    data = as.matrix(all_went_train_4[,2:7]),
    label = all_went_train_4$safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    nrounds = 1000,
    eta = 0.15,
    early_stopping_rounds = 10,
    max_depth = 1,
    min_child_weight = 2.5,
    gamma = 0.75,
    subsample = 0.9,
    verbose = 0                      # turn off training printout
)
all_went_test_4$xgb_4 <- predict(xgb.4_4, newdata = as.matrix(all_went_test_4[,2:7]))

log_loss(
    c(all_went_test_1$safe_out, all_went_test_2$safe_out, all_went_test_3$safe_out, all_went_test_4$safe_out) ,
    c(all_went_test_1$xgb_4, all_went_test_2$xgb_4, all_went_test_3$xgb_4, all_went_test_4$xgb_4)
)
[1] 0.1848132
xgb_params <- expand.grid(
  eta = c(0.05, 0.15),
  max_depth = c(1, 2, 3),
  gamma = c(0, 1),
  min_child_weight = c(1, 3, 5)
)

results <- data.frame()

for (i in 1:nrow(xgb_params)) {
  params <- list(
    objective = "binary:logistic",
    eval_metric = "logloss",
    eta = xgb_params$eta[i],
    max_depth = xgb_params$max_depth[i],
    min_child_weight = xgb_params$min_child_weight[i],
    gamma = xgb_params$gamma[i]
  )
  
  cv <- xgb.cv(
    params = params,
    data = as.matrix(all_went_data[,2:7]),
    label = all_went_data$safe_out,
    nrounds = 100,
    nfold = 5,
    verbose = 0
  )
  
  min_logloss <- min(cv$evaluation_log$test_logloss_mean)
  
  results <- rbind(results, cbind(xgb_params[i, ], logloss = min_logloss))
}

High correl = move down parameters      and vice versa
> cor(results$eta, results$logloss)
[1] -0.1280881
> cor(results$max_depth, results$logloss)
[1] -0.2205133
> cor(results$gamma, results$logloss)
[1] -0.199343
> cor(results$min_child_weight, results$logloss)
[1] 0.0885457
xgb_params_2 <- expand.grid(
  eta = c(0.1, 0.15),
  gamma = c(0.5, 1),
  min_child_weight = c(1, 2, 3)
)

results_2 <- data.frame()

for (i in 1:nrow(xgb_params_2)) {
  params_2 <- list(
    objective = "binary:logistic",
    eval_metric = "logloss",
    eta = xgb_params_2$eta[i],
    min_child_weight = xgb_params_2$min_child_weight[i],
    gamma = xgb_params_2$gamma[i]
  )
  
  cv_2 <- xgb.cv(
    params = params_2,
    data = as.matrix(all_went_data[,2:7]),
    label = all_went_data$safe_out,
    nrounds = 100,
    max_depth = 2,
    nfold = 5,
    verbose = 0
  )
  
  min_logloss_2 <- min(cv_2$evaluation_log$test_logloss_mean)
  
  results_2 <- rbind(results_2, cbind(xgb_params_2[i, ], logloss = min_logloss_2))
}

High correl = move down parameters      and vice versa
> cor(results_2$eta, results_2$logloss)
[1] 0.2799819
> cor(results_2$gamma, results_2$logloss)
[1] -0.278459
> cor(results_2$min_child_weight, results_2$logloss)
[1] -0.3453839
xgb.4_1 <- xgboost(
    data = as.matrix(all_went_train_1[,2:7]),
    label = all_went_train_1$safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    nrounds = 100,                   # number of boosting rounds
    max_depth = 2,                   # depth of trees
    eta = 0.16,                       # learning rate
    gamma = 0.75,
    min_child_weight = 3.6,
    verbose = 0                      # turn off training printout
)
all_went_test_1$xgb_4 <- predict(xgb.4_1, newdata = as.matrix(all_went_test_1[,2:7]))

xgb.4_2 <- xgboost(
    data = as.matrix(all_went_train_2[,2:7]),
    label = all_went_train_2$safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    nrounds = 100,                   # number of boosting rounds
    max_depth = 2,                   # depth of trees
    eta = 0.16,                       # learning rate
    gamma = 0.75,
    min_child_weight = 3.6,
    verbose = 0                      # turn off training printout
)
all_went_test_2$xgb_4 <- predict(xgb.4_2, newdata = as.matrix(all_went_test_2[,2:7]))

xgb.4_3 <- xgboost(
    data = as.matrix(all_went_train_3[,2:7]),
    label = all_went_train_3$safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    nrounds = 100,                   # number of boosting rounds
    max_depth = 2,                   # depth of trees
    eta = 0.16,                       # learning rate
    gamma = 0.75,
    min_child_weight = 3.6,
    verbose = 0                      # turn off training printout
)
all_went_test_3$xgb_4 <- predict(xgb.4_3, newdata = as.matrix(all_went_test_3[,2:7]))

xgb.4_4 <- xgboost(
    data = as.matrix(all_went_train_4[,2:7]),
    label = all_went_train_4$safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    nrounds = 100,                   # number of boosting rounds
    max_depth = 2,                   # depth of trees
    eta = 0.16,                       # learning rate
    gamma = 0.75,
    min_child_weight = 3.6,
    verbose = 0                      # turn off training printout
)
all_went_test_4$xgb_4 <- predict(xgb.4_4, newdata = as.matrix(all_went_test_4[,2:7]))

log_loss(
    c(all_went_test_1$safe_out, all_went_test_2$safe_out, all_went_test_3$safe_out, all_went_test_4$safe_out) ,
    c(all_went_test_1$xgb_4, all_went_test_2$xgb_4, all_went_test_3$xgb_4, all_went_test_4$xgb_4)
)
[1] 0.1682787

sum(
    abs( c(all_went_test_1$safe_out, all_went_test_2$safe_out, all_went_test_3$safe_out, all_went_test_4$safe_out) -
             ifelse(c(all_went_test_1$xgb_4, all_went_test_2$xgb_4, all_went_test_3$xgb_4, all_went_test_4$xgb_4) >= 0.5, 1, 0) )
)
[1] 35
rf_1 <- randomForest(as.factor(safe_out) ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, 
                         data = all_went_train_1, 
                         ntree = 100, 
                         importance = TRUE, 
                         probability = TRUE)
all_went_test_1$rf <- predict(rf_1, newdata = all_went_test_1, type = "prob")[, "1"]

rf_2 <- randomForest(as.factor(safe_out) ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, 
                         data = all_went_train_2, 
                         ntree = 100, 
                         importance = TRUE, 
                         probability = TRUE)
all_went_test_2$rf <- predict(rf_2, newdata = all_went_test_2, type = "prob")[, "1"]

rf_3 <- randomForest(as.factor(safe_out) ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, 
                         data = all_went_train_3, 
                         ntree = 100, 
                         importance = TRUE, 
                         probability = TRUE)
all_went_test_3$rf <- predict(rf_3, newdata = all_went_test_3, type = "prob")[, "1"]

rf_4 <- randomForest(as.factor(safe_out) ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, 
                         data = all_went_train_4, 
                         ntree = 100, 
                         importance = TRUE, 
                         probability = TRUE)
all_went_test_4$rf <- predict(rf_4, newdata = all_went_test_4, type = "prob")[, "1"]

log_loss(
    c(all_went_test_1$safe_out, all_went_test_2$safe_out, all_went_test_3$safe_out, all_went_test_4$safe_out) ,
    c(all_went_test_1$rf, all_went_test_2$rf, all_went_test_3$rf, all_went_test_4$rf)
)
[1] 0.2759139

sum(
    abs( c(all_went_test_1$safe_out, all_went_test_2$safe_out, all_went_test_3$safe_out, all_went_test_4$safe_out) -
             ifelse(c(all_went_test_1$rf, all_went_test_2$rf, all_went_test_3$rf, all_went_test_4$rf) >= 0.5, 1, 0) )
)
[1] 28
rf.2_1 <- randomForest(as.factor(safe_out) ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, 
                         data = all_went_train_1, 
                         ntree = 500, 
                         importance = TRUE, 
                         probability = TRUE)
all_went_test_1$rf_2 <- predict(rf.2_1, newdata = all_went_test_1, type = "prob")[, "1"]

rf.2_2 <- randomForest(as.factor(safe_out) ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, 
                         data = all_went_train_2, 
                         ntree = 500, 
                         importance = TRUE, 
                         probability = TRUE)
all_went_test_2$rf_2 <- predict(rf.2_2, newdata = all_went_test_2, type = "prob")[, "1"]

rf.2_3 <- randomForest(as.factor(safe_out) ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, 
                         data = all_went_train_3, 
                         ntree = 500, 
                         importance = TRUE, 
                         probability = TRUE)
all_went_test_3$rf_2 <- predict(rf.2_3, newdata = all_went_test_3, type = "prob")[, "1"]

rf.2_4 <- randomForest(as.factor(safe_out) ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, 
                         data = all_went_train_4, 
                         ntree = 500, 
                         importance = TRUE, 
                         probability = TRUE)
all_went_test_4$rf_2 <- predict(rf.2_4, newdata = all_went_test_4, type = "prob")[, "1"]

log_loss(
    c(all_went_test_1$safe_out, all_went_test_2$safe_out, all_went_test_3$safe_out, all_went_test_4$safe_out) ,
    c(all_went_test_1$rf_2, all_went_test_2$rf_2, all_went_test_3$rf_2, all_went_test_4$rf_2)
)
[1] 0.1625212

sum(
    abs( c(all_went_test_1$safe_out, all_went_test_2$safe_out, all_went_test_3$safe_out, all_went_test_4$safe_out) -
             ifelse(c(all_went_test_1$rf_2, all_went_test_2$rf_2, all_went_test_3$rf_2, all_went_test_4$rf_2) >= 0.5, 1, 0) )
)
[1] 28
rf.3_1 <- randomForest(as.factor(safe_out) ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, 
                         data = all_went_train_1, 
                         ntree = 1000, 
                         importance = TRUE, 
                         probability = TRUE)
all_went_test_1$rf_3 <- predict(rf.3_1, newdata = all_went_test_1, type = "prob")[, "1"]

rf.3_2 <- randomForest(as.factor(safe_out) ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, 
                         data = all_went_train_2, 
                         ntree = 1000, 
                         importance = TRUE, 
                         probability = TRUE)
all_went_test_2$rf_3 <- predict(rf.3_2, newdata = all_went_test_2, type = "prob")[, "1"]

rf.3_3 <- randomForest(as.factor(safe_out) ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, 
                         data = all_went_train_3, 
                         ntree = 1000, 
                         importance = TRUE, 
                         probability = TRUE)
all_went_test_3$rf_3 <- predict(rf.3_3, newdata = all_went_test_3, type = "prob")[, "1"]

rf.3_4 <- randomForest(as.factor(safe_out) ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, 
                         data = all_went_train_4, 
                         ntree = 1000, 
                         importance = TRUE, 
                         probability = TRUE)
all_went_test_4$rf_3 <- predict(rf.3_4, newdata = all_went_test_4, type = "prob")[, "1"]

log_loss(
    c(all_went_test_1$safe_out, all_went_test_2$safe_out, all_went_test_3$safe_out, all_went_test_4$safe_out) ,
    c(all_went_test_1$rf_3, all_went_test_2$rf_3, all_went_test_3$rf_3, all_went_test_4$rf_3)
)
[1] 0.1640204

sum(
    abs( c(all_went_test_1$safe_out, all_went_test_2$safe_out, all_went_test_3$safe_out, all_went_test_4$safe_out) -
             ifelse(c(all_went_test_1$rf_3, all_went_test_2$rf_3, all_went_test_3$rf_3, all_went_test_4$rf_3) >= 0.5, 1, 0) )
)
[1] 28
rf.4_1 <- randomForest(as.factor(safe_out) ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, 
                         data = all_went_train_1, 
                         ntree = 750, 
                         importance = TRUE, 
                         probability = TRUE)
all_went_test_1$rf_4 <- predict(rf.4_1, newdata = all_went_test_1, type = "prob")[, "1"]

rf.4_2 <- randomForest(as.factor(safe_out) ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, 
                         data = all_went_train_2, 
                         ntree = 750, 
                         importance = TRUE, 
                         probability = TRUE)
all_went_test_2$rf_4 <- predict(rf.4_2, newdata = all_went_test_2, type = "prob")[, "1"]

rf.4_3 <- randomForest(as.factor(safe_out) ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, 
                         data = all_went_train_3, 
                         ntree = 750, 
                         importance = TRUE, 
                         probability = TRUE)
all_went_test_3$rf_4 <- predict(rf.4_3, newdata = all_went_test_3, type = "prob")[, "1"]

rf.4_4 <- randomForest(as.factor(safe_out) ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, 
                         data = all_went_train_4, 
                         ntree = 750, 
                         importance = TRUE, 
                         probability = TRUE)
all_went_test_4$rf_4 <- predict(rf.4_4, newdata = all_went_test_4, type = "prob")[, "1"]

log_loss(
    c(all_went_test_1$safe_out, all_went_test_2$safe_out, all_went_test_3$safe_out, all_went_test_4$safe_out) ,
    c(all_went_test_1$rf_4, all_went_test_2$rf_4, all_went_test_3$rf_4, all_went_test_4$rf_4)
)
[1] 0.164647

sum(
    abs( c(all_went_test_1$safe_out, all_went_test_2$safe_out, all_went_test_3$safe_out, all_went_test_4$safe_out) -
             ifelse(c(all_went_test_1$rf_4, all_went_test_2$rf_4, all_went_test_3$rf_4, all_went_test_4$rf_4) >= 0.5, 1, 0) )
)
[1] 28
all_went_data_2 <- rbind(all_went_safe_data, all_went_out_data)

bayesian_function_2 <- function(eta, max_depth, subsample, gamma, min_child_weight) {
  
  params_2 <- list(
    objective = "binary:logistic",
    eval_metric = "logloss",
    eta = eta,
    max_depth = as.integer(max_depth),
    subsample = subsample,
    gamma = gamma,
    min_child_weight = min_child_weight
  )
  
  loglosses <- c()
  
  for (k in 1:4) {
    train_fold <- all_went_data[all_went_data_2$group != k, ]
    valid_fold <- all_went_data[all_went_data_2$group == k, ]
    
    dtrain <- xgb.DMatrix(data = as.matrix(train_fold[, 2:7]), label = train_fold$safe_out)
    dvalid <- xgb.DMatrix(data = as.matrix(valid_fold[, 2:7]), label = valid_fold$safe_out)
    
    cv_model <- xgb.train(
      params = params_2,
      data = dtrain,
      nrounds = 100,
      watchlist = list(val = dvalid),
      early_stopping_rounds = 10,
      verbose = 0
    )
    
    loglosses <- c(loglosses, cv_model$evaluation_log$val_logloss[cv_model$best_iteration])
  }
  
  best_logloss <- mean(loglosses)
  
  return(list(Score = -best_logloss))
}

bounds_2 <- list(
  eta = c(0.01, 0.2),
  max_depth = c(1L, 3L),
  subsample = c(0.75, 1),
  gamma = c(0, 2),
  min_child_weight = c(1, 10)
)

set.seed(123)
optObj_2 <- bayesOpt(
  FUN = bayesian_function_2,
  bounds = bounds_2,
  initPoints = 10,
  iters.n = 20,
  acq = "ucb",
  verbose = 0
)

getBestPars(optObj_2)
$eta
[1] 0.2

$max_depth
[1] 1

$subsample
[1] 0.8566987

$gamma
[1] 0

$min_child_weight
[1] 1
xgb.5_1 <- xgboost(
    data = as.matrix(all_went_train_1[,2:7]),
    label = all_went_train_1$safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    nrounds = 100,                   # number of boosting rounds
    max_depth = 1,                   # depth of trees
    eta = 0.2,                       # learning rate
    gamma = 0,
    subsample = 0.86,
    min_child_weight = 1,
    verbose = 0                      # turn off training printout
)
all_went_test_1$xgb_5 <- predict(xgb.5_1, newdata = as.matrix(all_went_test_1[,2:7]))

xgb.5_2 <- xgboost(
    data = as.matrix(all_went_train_2[,2:7]),
    label = all_went_train_2$safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    nrounds = 100,                   # number of boosting rounds
    max_depth = 1,                   # depth of trees
    eta = 0.2,                       # learning rate
    gamma = 0,
    subsample = 0.86,
    min_child_weight = 1,
    verbose = 0                      # turn off training printout
)
all_went_test_2$xgb_5 <- predict(xgb.5_2, newdata = as.matrix(all_went_test_2[,2:7]))

xgb.5_3 <- xgboost(
    data = as.matrix(all_went_train_3[,2:7]),
    label = all_went_train_3$safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    nrounds = 100,                   # number of boosting rounds
    max_depth = 1,                   # depth of trees
    eta = 0.2,                       # learning rate
    gamma = 0,
    subsample = 0.86,
    min_child_weight = 1,
    verbose = 0                      # turn off training printout
)
all_went_test_3$xgb_5 <- predict(xgb.5_3, newdata = as.matrix(all_went_test_3[,2:7]))

xgb.5_4 <- xgboost(
    data = as.matrix(all_went_train_4[,2:7]),
    label = all_went_train_4$safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    nrounds = 100,                   # number of boosting rounds
    max_depth = 1,                   # depth of trees
    eta = 0.2,                       # learning rate
    gamma = 0,
    subsample = 0.86,
    min_child_weight = 1,
    verbose = 0                      # turn off training printout
)
all_went_test_4$xgb_5 <- predict(xgb.5_4, newdata = as.matrix(all_went_test_4[,2:7]))

log_loss(
    c(all_went_test_1$safe_out, all_went_test_2$safe_out, all_went_test_3$safe_out, all_went_test_4$safe_out) ,
    c(all_went_test_1$xgb_5, all_went_test_2$xgb_5, all_went_test_3$xgb_5, all_went_test_4$xgb_5)
)
[1] 0.1664956

sum(
    abs( c(all_went_test_1$safe_out, all_went_test_2$safe_out, all_went_test_3$safe_out, all_went_test_4$safe_out) -
             ifelse(c(all_went_test_1$xgb_5, all_went_test_2$xgb_5, all_went_test_3$xgb_5, all_went_test_4$xgb_5) >= 0.5, 1, 0) )
)
[1] 35
xgb_params_2 <- expand.grid(
  max_depth = c(1, 2, 3, 4),
  min_child_weight = c(1, 2, 3, 4, 5)
)

results_2 <- data.frame()

for (i in 1:nrow(xgb_params_2)) {
  params_2 <- list(
    objective = "binary:logistic",
    eval_metric = "logloss",
    max_depth = xgb_params_2$max_depth[i],
    min_child_weight = xgb_params_2$min_child_weight[i]
  )
  
  cv_2 <- xgb.cv(
    params = params_2,
    data = as.matrix(all_went_data[,2:7]),
    label = all_went_data$safe_out,
    nrounds = 250,
    eta = 0.025,
    nfold = 5,
    verbose = 0
  )
  
  min_logloss <- min(cv_2$evaluation_log$test_logloss_mean)
  
  results_2 <- rbind(results_2, cbind(xgb_params_2[i, ], logloss = min_logloss))
}

> cor(results_2$logloss, results_2$max_depth)
[1] 0.3795422
> cor(results_2$logloss, results_2$min_child_weight)
[1] -0.02704552

Best was max_depth of 2, min_child_weight didn’t seem to matter much, go with 3
xgb_params_3 <- expand.grid(
  gamma = c(0, 0.25, 0.5, 0.75, 1, 1.25),
  early_stopping_rounds = c(3, 6, 9, 12, 15)
)

results_3 <- data.frame()

for (i in 1:nrow(xgb_params_3)) {
  params_3 <- list(
    objective = "binary:logistic",
    eval_metric = "logloss",
    gamma = xgb_params_3$gamma[i]
  )
  
  cv_3 <- xgb.cv(
    params = params_3,
    data = as.matrix(all_went_data[,2:7]),
    label = all_went_data$safe_out,
    nrounds = 250,
    eta = 0.025,
    max_depth = 2,
    min_child_weight = 3,
    early_stopping_rounds = xgb_params_3$early_stopping_rounds[i],
    nfold = 5,
    verbose = 0
  )
  
  min_logloss <- min(cv_3$evaluation_log$test_logloss_mean)
  
  results_3 <- rbind(results_3, cbind(xgb_params_3[i, ], logloss = min_logloss))
}

> cor(results_3$logloss, results_3$gamma)
[1] -0.1353625
> cor(results_3$logloss, results_3$early_stopping_rounds)
[1] -0.04224129

Best were higher gammas, higher stopping rounds, so we’ll go gamma = 1 and early_stopping_rounds = 10
xgb.6_1 <- xgboost(
    data = as.matrix(all_went_train_1[,2:7]),
    label = all_went_train_1$safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    eval_metric = "logloss",
    nrounds = 250,                   # number of boosting rounds
    max_depth = 2,                   # depth of trees
    eta = 0.025,                       # learning rate
    gamma = 1,
    early_stopping_rounds = 10,
    min_child_weight = 3,
    verbose = 0                      # turn off training printout
)
all_went_test_1$xgb_6 <- predict(xgb.6_1, newdata = as.matrix(all_went_test_1[,2:7]))

xgb.6_2 <- xgboost(
    data = as.matrix(all_went_train_2[,2:7]),
    label = all_went_train_2$safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    eval_metric = "logloss",
    nrounds = 250,                   # number of boosting rounds
    max_depth = 2,                   # depth of trees
    eta = 0.025,                       # learning rate
    gamma = 1,
    early_stopping_rounds = 10,
    min_child_weight = 3,
    verbose = 0                      # turn off training printout
)
all_went_test_2$xgb_6 <- predict(xgb.6_2, newdata = as.matrix(all_went_test_2[,2:7]))

xgb.6_3 <- xgboost(
    data = as.matrix(all_went_train_3[,2:7]),
    label = all_went_train_3$safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    eval_metric = "logloss",
    nrounds = 250,                   # number of boosting rounds
    max_depth = 2,                   # depth of trees
    eta = 0.025,                       # learning rate
    gamma = 1,
    early_stopping_rounds = 10,
    min_child_weight = 3,
    verbose = 0                      # turn off training printout
)
all_went_test_3$xgb_6 <- predict(xgb.6_3, newdata = as.matrix(all_went_test_3[,2:7]))

xgb.6_4 <- xgboost(
    data = as.matrix(all_went_train_4[,2:7]),
    label = all_went_train_4$safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    eval_metric = "logloss",
    nrounds = 250,                   # number of boosting rounds
    max_depth = 2,                   # depth of trees
    eta = 0.025,                       # learning rate
    gamma = 1,
    early_stopping_rounds = 10,
    min_child_weight = 3,
    verbose = 0                      # turn off training printout
)
all_went_test_4$xgb_6 <- predict(xgb.6_4, newdata = as.matrix(all_went_test_4[,2:7]))

log_loss(
    c(all_went_test_1$safe_out, all_went_test_2$safe_out, all_went_test_3$safe_out, all_went_test_4$safe_out) ,
    c(all_went_test_1$xgb_6, all_went_test_2$xgb_6, all_went_test_3$xgb_6, all_went_test_4$xgb_6)
)
[1] 0.1667822

sum(
    abs( c(all_went_test_1$safe_out, all_went_test_2$safe_out, all_went_test_3$safe_out, all_went_test_4$safe_out) -
             ifelse(c(all_went_test_1$xgb_6, all_went_test_2$xgb_6, all_went_test_3$xgb_6, all_went_test_4$xgb_6) >= 0.5, 1, 0) )
)
[1] 35
xgb_params_4 <- expand.grid(
  gamma = c(0, 0.33, 0.67, 1, 1.33, 1.67),
  early_stopping_rounds = c(6, 8, 10, 12),
  eta = c(0.8, 1, 1.2)
)

results_4 <- data.frame()

for (i in 1:nrow(xgb_params_4)) {
  params_4 <- list(
    objective = "binary:logistic",
    eval_metric = "logloss",
    gamma = xgb_params_4$gamma[i]
  )
  
  cv_4 <- xgb.cv(
    params = params_4,
    data = as.matrix(all_went_data[,2:7]),
    label = all_went_data$safe_out,
    nrounds = 100,
    eta = xgb_params_4$eta[i],
    max_depth = 2,
    early_stopping_rounds = xgb_params_4$early_stopping_rounds[i],
    nfold = 5,
    verbose = 0
  )
  
  min_logloss <- min(cv_4$evaluation_log$test_logloss_mean)
  
  results_4 <- rbind(results_4, cbind(xgb_params_4[i, ], logloss = min_logloss))
}

> cor(results_4$logloss, results_4$gamma)
[1] -0.009743481
> cor(results_4$logloss, results_4$early_stopping_rounds)
[1] -0.210549
> cor(results_4$logloss, results_4$eta)
[1] 0.4471328
xgb.7_1 <- xgboost(
    data = as.matrix(all_went_train_1[,2:7]),
    label = all_went_train_1$safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    nrounds = 100,                   # number of boosting rounds
    max_depth = 2,                   # depth of trees
    eta = 0.08,                       # learning rate
    early_stopping_rounds = 10,
    verbose = 0                      # turn off training printout
)
all_went_test_1$xgb_7 <- predict(xgb.7_1, newdata = as.matrix(all_went_test_1[,2:7]))

xgb.7_2 <- xgboost(
    data = as.matrix(all_went_train_2[,2:7]),
    label = all_went_train_2$safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    nrounds = 100,                   # number of boosting rounds
    max_depth = 2,                   # depth of trees
    eta = 0.08,                       # learning rate
    early_stopping_rounds = 10,
    verbose = 0                      # turn off training printout
)
all_went_test_2$xgb_7 <- predict(xgb.7_2, newdata = as.matrix(all_went_test_2[,2:7]))

xgb.7_3 <- xgboost(
    data = as.matrix(all_went_train_3[,2:7]),
    label = all_went_train_3$safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    nrounds = 100,                   # number of boosting rounds
    max_depth = 2,                   # depth of trees
    eta = 0.08,                       # learning rate
    early_stopping_rounds = 10,
    verbose = 0                      # turn off training printout
)
all_went_test_3$xgb_7 <- predict(xgb.7_3, newdata = as.matrix(all_went_test_3[,2:7]))

xgb.7_4 <- xgboost(
    data = as.matrix(all_went_train_4[,2:7]),
    label = all_went_train_4$safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    nrounds = 100,                   # number of boosting rounds
    max_depth = 2,                   # depth of trees
    eta = 0.08,                       # learning rate
    early_stopping_rounds = 10,
    verbose = 0                      # turn off training printout
)
all_went_test_4$xgb_7 <- predict(xgb.7_4, newdata = as.matrix(all_went_test_4[,2:7]))

log_loss(
    c(all_went_test_1$safe_out, all_went_test_2$safe_out, all_went_test_3$safe_out, all_went_test_4$safe_out) ,
    c(all_went_test_1$xgb_7, all_went_test_2$xgb_7, all_went_test_3$xgb_7, all_went_test_4$xgb_7)
)
[1] 0.1592561

sum(
    abs( c(all_went_test_1$safe_out, all_went_test_2$safe_out, all_went_test_3$safe_out, all_went_test_4$safe_out) -
             ifelse(c(all_went_test_1$xgb_7, all_went_test_2$xgb_7, all_went_test_3$xgb_7, all_went_test_4$xgb_7) >= 0.5, 1, 0) )
)
[1] 29



xgb_params <- expand.grid(
  max_depth = c(1, 2, 3, 4),
  min_child_weight = c(1, 2, 3, 4, 5)
)

results <- data.frame()

for (i in 1:nrow(xgb_params)) {

  strat_folds <- createFolds(all_went_data$safe_out, k = 5, list = TRUE, returnTrain = FALSE)

  params <- list(
    objective = "binary:logistic",
    eval_metric = "logloss",
    max_depth = xgb_params$max_depth[i],
    min_child_weight = xgb_params$min_child_weight[i]
  )
  
  cv <- xgb.cv(
    params = params,
    data = as.matrix(all_went_data[,2:7]),
    label = all_went_data$safe_out,
    nrounds = 1000,
    eta = 0.15,
    early_stopping_rounds = 10,
    folds = strat_folds,
    verbose = 0
  )
  
  min_logloss <- min(cv$evaluation_log$test_logloss_mean)
  results <- rbind(results, cbind(xgb_params[i, ], logloss = min_logloss))
}

AFTER FIRST ROUND
md_mcw_res <- results

AFTER SECOND+ ROUND
md_mcw_res <- left_join(md_mcw_res, results, by = c("max_depth", "min_child_weight"))

md_mcw_res <- md_mcw_res %>%
    mutate(avg = rowMeans(md_mcw_res[, 3:7]) )

ggplot(md_mcw_res, aes(x = max_depth, y = min_child_weight, color = avg)) +
  geom_point() +                # Add points
  scale_color_gradient(low = "green", high = "red")

max_depth = 2, min_child_weight = 2
xgb_params <- expand.grid(
  nrounds = c(300, 500, 750, 1000),
  gamma = c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5)
)

results <- data.frame()

for (i in 1:nrow(xgb_params)) {

  strat_folds <- createFolds(all_went_data$safe_out, k = 5, list = TRUE, returnTrain = FALSE)

  params <- list(
    objective = "binary:logistic",
    eval_metric = "logloss",
    gamma = xgb_params$gamma[i]
  )
  
  cv <- xgb.cv(
    params = params,
    data = as.matrix(all_went_data[,2:7]),
    label = all_went_data$safe_out,
    nrounds = xgb_params$nrounds[i],
    eta = 0.15,
    early_stopping_rounds = 10,
    max_depth = 2,
    min_child_weight = 2,
    folds = strat_folds,
    verbose = 0
  )
  
  min_logloss <- min(cv$evaluation_log$test_logloss_mean)
  results <- rbind(results, cbind(xgb_params[i, ], logloss = min_logloss))
}

AFTER FIRST ROUND
rounds_gamma_res_2 <- results

AFTER SECOND+ ROUND
rounds_gamma_res_2 <- left_join(rounds_gamma_res_2, results, by = c("nrounds", "gamma"))

rounds_gamma_res_2 <- rounds_gamma_res_2 %>%
    mutate(avg = rowMeans(rounds_gamma_res_2[, 3:7]) )

ggplot(rounds_gamma_res_2, aes(x = nrounds, y = gamma, color = avg)) +
  geom_point() +                # Add points
  scale_color_gradient(low = "green", high = "red")

0 gamma and 750 rounds
xgb_params <- expand.grid(
  subsample = c(0.5, 0.6, 0.7, 0.8, 0.9, 1),
  colsample_bytree = c(0.5, 0.6, 0.7, 0.8, 0.9, 1)
)

results <- data.frame()

for (i in 1:nrow(xgb_params)) {

  strat_folds <- createFolds(all_went_data$safe_out, k = 5, list = TRUE, returnTrain = FALSE)

  params <- list(
    objective = "binary:logistic",
    eval_metric = "logloss",
    subsample = xgb_params$subsample[i],
    colsample_bytree = xgb_params$colsample_bytree[i]
  )
  
  cv <- xgb.cv(
    params = params,
    data = as.matrix(all_went_data[,2:7]),
    label = all_went_data$safe_out,
    nrounds = 750,
    eta = 0.15,
    early_stopping_rounds = 10,
    max_depth = 2,
    min_child_weight = 2,
    gamma = 0,
    folds = strat_folds,
    verbose = 0
  )
  
  min_logloss <- min(cv$evaluation_log$test_logloss_mean)
  results <- rbind(results, cbind(xgb_params[i, ], logloss = min_logloss))
}

AFTER FIRST ROUND
subsample_res_2 <- results

AFTER SECOND+ ROUND
subsample_res_2 <- left_join(subsample_res_2, results, by = c("subsample", "colsample_bytree"))

subsample_res_2 <- subsample_res_2 %>%
    mutate(avg = rowMeans(subsample_res_2[, 3:7]) )

ggplot(subsample_res_2, aes(x = subsample, y = colsample_bytree, color = avg)) +
  geom_point() +                # Add points
  scale_color_gradient(low = "green", high = "red")

subsample = 0.9 and colsample_bytree = 0.9
xgb_params <- expand.grid(
  lambda = c(0, 1, 2, 3, 4, 5),
  max_delta_step = c(0, 2, 4, 6, 8, 10)
)

results <- data.frame()

for (i in 1:nrow(xgb_params)) {

  strat_folds <- createFolds(all_went_data$safe_out, k = 5, list = TRUE, returnTrain = FALSE)

  params <- list(
    objective = "binary:logistic",
    eval_metric = "logloss",
    lambda = xgb_params$lambda[i],
    max_delta_step = xgb_params$max_delta_step[i]
  )
  
  cv <- xgb.cv(
    params = params,
    data = as.matrix(all_went_data[,2:7]),
    label = all_went_data$safe_out,
    nrounds = 750,
    eta = 0.15,
    early_stopping_rounds = 10,
    max_depth = 2,
    min_child_weight = 2,
    gamma = 0,
    subsample = 0.9,
    colsample_bytree = 0.9,
    folds = strat_folds,
    verbose = 0
  )
  
  min_logloss <- min(cv$evaluation_log$test_logloss_mean)
  results <- rbind(results, cbind(xgb_params[i, ], logloss = min_logloss))
}

AFTER FIRST ROUND
lambda_mds_res_2 <- results

AFTER SECOND+ ROUND
lambda_mds_res_2 <- left_join(lambda_mds_res_2, results, by = c("lambda", "max_delta_step"))

lambda_mds_res_2 <- lambda_mds_res_2 %>%
    mutate(avg = rowMeans(lambda_mds_res_2[, 3:7]) )

ggplot(lambda_mds_res_2, aes(x = lambda, y = max_delta_step, color = avg)) +
  geom_point() +                # Add points
  scale_color_gradient(low = "green", high = "red")

lambda = 4, max_delta_step = 8
xgb.5_1 <- xgboost(
    data = as.matrix(all_went_train_1[,2:7]),
    label = all_went_train_1$safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    nrounds = 750,
    eta = 0.15,
    early_stopping_rounds = 10,
    max_depth = 2,
    min_child_weight = 2,
    subsample = 0.9,
    colsample_bytree = 0.9,
    lambda = 4,
    max_delta_step = 8,
    verbose = 0                      # turn off training printout
)
all_went_test_1$xgb_5 <- predict(xgb.5_1, newdata = as.matrix(all_went_test_1[,2:7]))

xgb.5_2 <- xgboost(
    data = as.matrix(all_went_train_2[,2:7]),
    label = all_went_train_2$safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    nrounds = 750,
    eta = 0.15,
    early_stopping_rounds = 10,
    max_depth = 2,
    min_child_weight = 2,
    subsample = 0.9,
    colsample_bytree = 0.9,
    lambda = 4,
    max_delta_step = 8,
    verbose = 0                      # turn off training printout
)
all_went_test_2$xgb_5 <- predict(xgb.5_2, newdata = as.matrix(all_went_test_2[,2:7]))

xgb.5_3 <- xgboost(
    data = as.matrix(all_went_train_3[,2:7]),
    label = all_went_train_3$safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    nrounds = 750,
    eta = 0.15,
    early_stopping_rounds = 10,
    max_depth = 2,
    min_child_weight = 2,
    subsample = 0.9,
    colsample_bytree = 0.9,
    lambda = 4,
    max_delta_step = 8,
    verbose = 0                      # turn off training printout
)
all_went_test_3$xgb_5 <- predict(xgb.5_3, newdata = as.matrix(all_went_test_3[,2:7]))

xgb.5_4 <- xgboost(
    data = as.matrix(all_went_train_4[,2:7]),
    label = all_went_train_4$safe_out,
    objective = "binary:logistic",   # logistic loss for binary classification
    nrounds = 750,
    eta = 0.15,
    early_stopping_rounds = 10,
    max_depth = 2,
    min_child_weight = 2,
    subsample = 0.9,
    colsample_bytree = 0.9,
    lambda = 4,
    max_delta_step = 8,
    verbose = 0                      # turn off training printout
)
all_went_test_4$xgb_5 <- predict(xgb.5_4, newdata = as.matrix(all_went_test_4[,2:7]))

log_loss(
    c(all_went_test_1$safe_out, all_went_test_2$safe_out, all_went_test_3$safe_out, all_went_test_4$safe_out) ,
    c(all_went_test_1$xgb_5, all_went_test_2$xgb_5, all_went_test_3$xgb_5, all_went_test_4$xgb_5)
)
[1] 0.2075284
xgb_params <- expand.grid(
  nrounds = c(750, 1000),
  gamma = c(0, 1),
  max_depth = c(1, 2),
  min_child_weight = c(2, 3),
  eta = c(0.1, 0.2)
)

results <- data.frame()

for (i in 1:nrow(xgb_params)) {

  strat_folds <- createFolds(all_went_data$safe_out, k = 5, list = TRUE, returnTrain = FALSE)

  params <- list(
    objective = "binary:logistic",
    eval_metric = "logloss",
    gamma = xgb_params$gamma[i],
    max_depth = xgb_params$max_depth[i],
    min_child_weight = xgb_params$min_child_weight[i],
    eta = xgb_params$eta[i]
  )
  
  cv <- xgb.cv(
    params = params,
    data = as.matrix(all_went_data[,2:7]),
    label = all_went_data$safe_out,
    nrounds = xgb_params$nrounds[i],
    folds = strat_folds,
    verbose = 0
  )
  
  min_logloss <- min(cv$evaluation_log$test_logloss_mean)
  results <- rbind(results, cbind(xgb_params[i, ], logloss = min_logloss))
}

AFTER FIRST ROUND
try_res <- results

AFTER SECOND+ ROUND
try_res <- left_join(try_res, results, by = c("nrounds", "gamma", "max_depth", "min_child_weight", "eta"))

try_res <- try_res %>%
    mutate(avg = rowMeans(try_res[, 6:10]) )

> cor(try_res$nrounds, try_res$avg)
[1] -0.1919057
> cor(try_res$gamma, try_res$avg)
[1] 0.03701839
> cor(try_res$max_depth, try_res$avg)
[1] 0.03410714
> cor(try_res$min_child_weight, try_res$avg)
[1] 0.1133519
> cor(try_res$eta, try_res$avg)
[1] -0.4745237
xgb_1_results <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$safe_out, k = 5, list = TRUE, returnTrain = FALSE)
  
  cv <- xgb.cv(
    data = as.matrix(all_went_data[,2:7]),
    label = all_went_data$safe_out,
    objective = "binary:logistic",
    eval_metric = "logloss",
    nrounds = 1000,
    eta = 0.15,
    early_stopping_rounds = 10,
    max_depth = 1,
    min_child_weight = 2.5,
    gamma = 0.75,
    subsample = 0.9,
    verbose = 0,
    folds = strat_folds 
  )
  
  min_log_loss <- min(cv$evaluation_log$test_logloss_mean)
  
  # Save in dataset
  xgb_1_results <- bind_rows(xgb_1_results, data.frame(test_num = i, logloss = min_log_loss))
}

> mean(xgb_1_results$logloss)
[1] 0.1820741
> sd(xgb_1_results$logloss)
[1] 0.00540917
xgb_2_results <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$safe_out, k = 5, list = TRUE, returnTrain = FALSE)
  
  cv <- xgb.cv(
    data = as.matrix(all_went_data[,2:7]),
    label = all_went_data$safe_out,
    objective = "binary:logistic",
    eval_metric = "logloss",
    nrounds = 750,
    eta = 0.15,
    early_stopping_rounds = 10,
    max_depth = 2,
    min_child_weight = 2,
    subsample = 0.9,
    colsample_bytree = 0.9,
    lambda = 4,
    max_delta_step = 8,
    verbose = 0,
    folds = strat_folds 
  )
  
  min_log_loss <- min(cv$evaluation_log$test_logloss_mean)
  
  # Save in dataset
  xgb_2_results <- bind_rows(xgb_2_results, data.frame(test_num = i, logloss = min_log_loss))
}

> mean(xgb_2_results$logloss)
[1] 0.1828643
> sd(xgb_2_results$logloss)
[1] 0.005098991
xgb_3_results <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$safe_out, k = 5, list = TRUE, returnTrain = FALSE)
  
  cv <- xgb.cv(
    data = as.matrix(all_went_data[,2:7]),
    label = all_went_data$safe_out,
    objective = "binary:logistic",
    eval_metric = "logloss",
    nrounds = 1000,
    eta = 0.15,
    early_stopping_rounds = 10,
    max_depth = 1,
    min_child_weight = 2.5,
    gamma = 0.75,
    verbose = 0,
    folds = strat_folds 
  )
  
  min_log_loss <- min(cv$evaluation_log$test_logloss_mean)
  
  # Save in dataset
  xgb_3_results <- bind_rows(xgb_3_results, data.frame(test_num = i, logloss = min_log_loss))
}

> mean(xgb_3_results$logloss)
[1] 0.1822199
> sd(xgb_3_results$logloss)
[1] 0.005895042
xgb_4_results <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$safe_out, k = 5, list = TRUE, returnTrain = FALSE)
  
  cv <- xgb.cv(
    data = as.matrix(all_went_data[,2:7]),
    label = all_went_data$safe_out,
    objective = "binary:logistic",
    eval_metric = "logloss",
    nrounds = 750,
    eta = 0.15,
    early_stopping_rounds = 10,
    max_depth = 2,
    min_child_weight = 2,
    verbose = 0,
    folds = strat_folds 
  )
  
  min_log_loss <- min(cv$evaluation_log$test_logloss_mean)
  
  # Save in dataset
  xgb_4_results <- bind_rows(xgb_4_results, data.frame(test_num = i, logloss = min_log_loss))
}

> mean(xgb_4_results$logloss)
[1] 0.1813718
> sd(xgb_4_results$logloss)
[1] 0.005654729
glm_results <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$safe_out, k = 5, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    glm <- glm(safe_out ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(glm, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$safe_out, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  glm_results <- bind_rows(glm_results, data.frame(test_num = i, logloss = avg_log_losses))
}

> mean(glm_results$logloss)
[1] 0.1632063
> sd(glm_results$logloss)
[1] 0.004688407
gam_results <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$safe_out, k = 5, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam <- gam(safe_out ~ s(run_dist) + s(OF_dist) + s(run_speed) + s(top_speed) + s(OF_momentum_home) + s(OF_momentum_side), data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$safe_out, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  gam_results <- bind_rows(gam_results, data.frame(test_num = i, logloss = avg_log_losses))
}

> mean(gam_results$logloss)
[1] 0.1782632
> sd(gam_results$logloss)
[1] 0.01376005
gam_2_results <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$safe_out, k = 5, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam_2 <- gam(safe_out ~ s(run_dist) + s(OF_dist) + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam_2, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$safe_out, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  gam_2_results <- bind_rows(gam_2_results, data.frame(test_num = i, logloss = avg_log_losses))
}

> mean(gam_2_results$logloss)
[1] 0.1635263
> sd(gam_2_results$logloss)
[1] 0.006182133
gam_3_results <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$safe_out, k = 5, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam_3 <- gam(safe_out ~ s(run_dist) + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam_3, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$safe_out, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  gam_3_results <- bind_rows(gam_3_results, data.frame(test_num = i, logloss = avg_log_losses))
}

> mean(gam_3_results$logloss)
[1] 0.1637304
> sd(gam_3_results$logloss)
[1] 0.005998743
xgb_5_results <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$safe_out, k = 5, list = TRUE, returnTrain = FALSE)
  
  cv <- xgb.cv(
    data = as.matrix(all_went_data[,2:7]),
    label = all_went_data$safe_out,
    objective = "binary:logistic",
    eval_metric = "logloss",
    nrounds = 100,
    max_depth = 2,
    eta = 0.1, 
    verbose = 0,
    folds = strat_folds 
  )
  
  min_log_loss <- min(cv$evaluation_log$test_logloss_mean)
  
  # Save in dataset
  xgb_5_results <- bind_rows(xgb_5_results, data.frame(test_num = i, logloss = min_log_loss))
}

> mean(xgb_5_results$logloss)
[1] 0.1792343
> sd(xgb_5_results$logloss)
[1] 0.005194403
gam_4_results <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$safe_out, k = 5, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam_4 <- gam(safe_out ~ s(run_dist) + s(OF_dist) + run_speed + top_speed + OF_momentum_home + OF_momentum_side + ti(run_dist, OF_dist), data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam_4, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$safe_out, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  gam_4_results <- bind_rows(gam_4_results, data.frame(test_num = i, logloss = avg_log_losses))
}

> mean(gam_4_results$logloss)
[1] 0.1682762
> sd(gam_4_results$logloss)
[1] 0.007449625
glm_2_results <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$safe_out, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    glm_2 <- glm(safe_out ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(glm_2, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$safe_out, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  glm_2_results <- bind_rows(glm_2_results, data.frame(test_num = i, logloss = avg_log_losses))
}

> mean(glm_2_results$logloss)
[1] 0.1624557
> sd(glm_2_results$logloss)
[1] 0.003965153
gam_5_results <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$safe_out, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam_5 <- gam(safe_out ~ s(run_dist) + s(OF_dist) + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam_5, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$safe_out, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  gam_5_results <- bind_rows(gam_5_results, data.frame(test_num = i, logloss = avg_log_losses))
}

> mean(gam_5_results$logloss)
[1] 0.161838
> sd(gam_5_results$logloss)
[1] 0.003966323
gam_6_results <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$safe_out, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam_6 <- gam(safe_out ~ s(run_dist) + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam_6, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$safe_out, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  gam_6_results <- bind_rows(gam_6_results, data.frame(test_num = i, logloss = avg_log_losses))
}

> mean(gam_6_results$logloss)
[1] 0.1618273
> sd(gam_6_results$logloss)
[1] 0.003121895
gam_7_results <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$safe_out, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam_7 <- gam(safe_out ~ run_dist + s(OF_dist) + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam_7, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$safe_out, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  gam_7_results <- bind_rows(gam_7_results, data.frame(test_num = i, logloss = avg_log_losses))
}

> mean(gam_7_results$logloss)
[1] 0.162725
> sd(gam_7_results$logloss)
[1] 0.002937188
gam_8_results <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$safe_out, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam_8 <- gam(safe_out ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side + ti(run_dist, run_speed) + ti(run_dist, top_speed), data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam_8, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$safe_out, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  gam_8_results <- bind_rows(gam_8_results, data.frame(test_num = i, logloss = avg_log_losses))
}

> mean(gam_8_results$logloss)
[1] 0.1635012
> sd(gam_8_results$logloss)
[1] 0.004959418
gam_9_results <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$safe_out, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam_9 <- gam(safe_out ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side + (run_dist / run_speed) + (run_dist / top_speed), data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam_9, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$safe_out, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  gam_9_results <- bind_rows(gam_9_results, data.frame(test_num = i, logloss = avg_log_losses))
}

> mean(gam_9_results$logloss)
[1] 0.1615811
> sd(gam_9_results$logloss)
[1] 0.003309449
gam_10_results <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$safe_out, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam_10 <- gam(safe_out ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side + te(run_speed_time, top_speed_time), data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam_10, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$safe_out, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  gam_10_results <- bind_rows(gam_10_results, data.frame(test_num = i, logloss = avg_log_losses))
}

> mean(gam_10_results$logloss)
[1] 0.1587154
> sd(gam_10_results$logloss)
[1] 0.007264696
all_final_data_4 <- all_final_data_4 %>%
    mutate(run_speed_time = run_dist / run_speed) %>%
    mutate(top_speed_time = run_dist / top_speed)

all_went_data <- all_went_data %>%
    mutate(run_speed_time = run_dist / run_speed) %>%
    mutate(top_speed_time = run_dist / top_speed)
gam_11_results <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$safe_out, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam_11 <- gam(safe_out ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side + s(run_speed_time) + s(top_speed_time), data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam_11, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$safe_out, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  gam_11_results <- bind_rows(gam_11_results, data.frame(test_num = i, logloss = avg_log_losses))
}

> mean(gam_11_results$logloss)
[1] 0.1600262
> sd(gam_11_results$logloss)
[1] 0.003558354
gam_12_results <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$safe_out, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam_12 <- gam(safe_out ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side + s(run_speed_time) + top_speed_time, data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam_12, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$safe_out, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  gam_12_results <- bind_rows(gam_12_results, data.frame(test_num = i, logloss = avg_log_losses))
}

> mean(gam_12_results$logloss)
[1] 0.1595168
> sd(gam_12_results$logloss)
[1] 0.003993203
gam_13_results <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$safe_out, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam_13 <- gam(safe_out ~ run_dist + OF_dist + OF_momentum_home + run_speed + top_speed + OF_momentum_side + ti(run_dist, top_speed), data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam_13, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$safe_out, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  gam_13_results <- bind_rows(gam_13_results, data.frame(test_num = i, logloss = avg_log_losses))
}

> mean(gam_13_results$logloss)
[1] 0.1637908
> sd(gam_13_results$logloss)
[1] 0.004862865
gam_14_results <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$safe_out, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam_14 <- gam(safe_out ~ te(run_dist, OF_dist) + run_speed + top_speed + OF_momentum_home + OF_momentum_side + ti(run_dist, top_speed), data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam_14, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$safe_out, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  gam_14_results <- bind_rows(gam_14_results, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(gam_14_results$logloss)
sd(gam_14_results$logloss)
glm_3_results <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$safe_out, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    glm_3 <- glm(safe_out ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side + run_dist:run_speed:top_speed, data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(glm_3, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$safe_out, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  glm_3_results <- bind_rows(glm_3_results, data.frame(test_num = i, logloss = avg_log_losses))
}

> mean(glm_3_results$logloss)
[1] 0.1626715
> sd(glm_3_results$logloss)
[1] 0.003276978
glm_4_results <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$safe_out, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    glm_4 <- glm(safe_out ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side + run_dist:top_speed, data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(glm_4, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$safe_out, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  glm_4_results <- bind_rows(glm_4_results, data.frame(test_num = i, logloss = avg_log_losses))
}

> mean(glm_4_results$logloss)
[1] 0.1601077
> sd(glm_4_results$logloss)
[1] 0.00272384
final_gam_model <- gam(safe_out ~ s(run_dist) + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = all_went_data, family = binomial)

all_final_data_4$gam_prob <- predict(final_gam_model, newdata = all_final_data_4, type = "response")

summary(final_gam_model)

Family: binomial 
Link function: logit 

Formula:
safe_out ~ s(run_dist) + OF_dist + run_speed + top_speed + OF_momentum_home + 
    OF_momentum_side

Parametric coefficients:
                  Estimate Std. Error z value Pr(>|z|)    
(Intercept)      -5.207724   4.489590  -1.160   0.2461    
OF_dist           0.039195   0.009072   4.320 1.56e-05 ***
run_speed         0.016925   0.122043   0.139   0.8897    
top_speed        -0.028368   0.151799  -0.187   0.8518    
OF_momentum_home -0.053750   0.032510  -1.653   0.0983 .  
OF_momentum_side  0.008784   0.038321   0.229   0.8187    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Approximate significance of smooth terms:
              edf Ref.df Chi.sq p-value    
s(run_dist) 1.965  2.469   38.7  <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

R-sq.(adj) =  0.233   Deviance explained = 37.1%
UBRE = -0.68389  Scale est. = 1         n = 561
final_glm_model <- glm(safe_out ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = all_went_data, family = binomial)

summary(final_glm_model)

all:
glm(formula = safe_out ~ run_dist + OF_dist + run_speed + top_speed + 
    OF_momentum_home + OF_momentum_side, family = binomial, data = all_went_data)

Coefficients:
                   Estimate Std. Error z value Pr(>|z|)    
(Intercept)       5.2996946  4.3396801   1.221   0.2220    
run_dist         -0.1185783  0.0189230  -6.266  3.7e-10 ***
OF_dist           0.0373912  0.0088895   4.206  2.6e-05 ***
run_speed         0.0256666  0.1203163   0.213   0.8311    
top_speed        -0.0343819  0.1539940  -0.223   0.8233    
OF_momentum_home -0.0656118  0.0320057  -2.050   0.0404 *  
OF_momentum_side  0.0001999  0.0380729   0.005   0.9958    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 256.52  on 560  degrees of freedom
Residual deviance: 166.01  on 554  degrees of freedom
AIC: 180.01

Number of Fisher Scoring iterations: 8
didnt_go_check <- all_final_data_4[,c(1, 8, 11, 24)] %>%
    filter(score_chance == -1)

didnt_go_check <- didnt_go_check %>%
    mutate(baserunner = scoring_from + 10)

didnt_go_pos <- player_pos %>%
    # Create composite key
    mutate(key = paste(game_str, play_id, player_position, sep = "_")) %>%
    filter(
        key %in% (didnt_go_check %>%
           mutate(key = paste(play_key, baserunner, sep = "_")) %>%
           pull(key))
    ) %>%
    group_by(key) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    dplyr::select(-c("Season", "HomeTeam", "AwayTeam", "Day")) %>%
    collect()

didnt_go_pos <- didnt_go_pos %>%
    mutate(dist = sqrt(field_x^2 + field_y^2))

didnt_go_pos <- left_join(didnt_go_pos, stats[,c(1:2,8)], by = c("game_str", "play_id"))

didnt_go_pos <- left_join(didnt_go_pos, stats_1st[,c(1:2,8)], by = c("game_str", "play_id"))

didnt_go_pos <- didnt_go_pos %>%
    mutate(home_dist.x = ifelse(is.na(home_dist.x), 0, home_dist.x))

didnt_go_pos <- didnt_go_pos %>%
    mutate(home_dist.y = ifelse(is.na(home_dist.y), 0, home_dist.y))

didnt_go_pos <- didnt_go_pos %>%
    mutate(went = ifelse(dist == home_dist.x, 1, ifelse(dist == home_dist.y, 1, 0)))

didnt_go_pos <- didnt_go_pos %>%
    mutate(key = paste(game_str, play_id, player_position, sep = "_"))

didnt_go_check <- didnt_go_check %>%
    mutate(key = paste(play_key, baserunner, sep = "_"))

didnt_go_pos <- left_join(didnt_go_pos, didnt_go_check[,c(4, 6)], by = "key")

write.csv(all_final_data_4, "all_final_data_4.csv", row.names = FALSE)

Deleted
animate_play("y1_d049_STK_QEA", 304) from 2nd
animate_play("2_d073_XFE_RZQ", 100) from 1st
animate_play("y1_d057_OXG_QEA", 96) from 1st
> sum( ifelse(!is.na(all_final_data_4$safe_out), all_final_data_4$gam_prob, 0) )
[1] 527
527 of those who went were safe, check

> sum(all_final_data_4$gam_prob)
[1] 671.7764
Guesses 144.77 of 507 who didn’t go would’ve been safe

all_final_data_4 <- all_final_data_4 %>%
    mutate(went_home = ifelse(!is.na(safe_out), 1, 0)) %>%
    mutate(should_go = ifelse(gam_prob >= prob_to_go, 1, 0)) %>%
    mutate(correct_decision = ifelse(went_home == should_go, 1, 0))

> mean(all_final_data_4$correct_decision)
[1] 0.8913858

confusion_matrix <- table(Predicted = all_final_data_4$should_go, Actual = all_final_data_4$went_home)
print(confusion_matrix)
        Actual
Predicted   0   1
        0 412  21
        1  95 540

A lot more “should go but didn’t”, than “shouldn’t go but did”
all_final_data_4 <- all_final_data_4 %>%
    filter(top_speed > 15)

new_glm_model <- glm(safe_out ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = all_went_data, family = binomial)

all_final_data_4$new_glm_prob <- predict(new_glm_model, newdata = all_final_data_4, type = "response")

summary(new_glm_model)

new_gam_model <- gam(safe_out ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = all_went_data, family = binomial)

all_final_data_4$new_gam_prob <- predict(new_gam_model, newdata = all_final_data_4, type = "response")

summary(new_gam_model)

all_final_data_4 <- all_final_data_4[,1:29]
SMOTE function - RDocumentation

all_went_smote <- SMOTE(X = all_went_data[, 2:7], 
    target = all_went_data$safe_out, 
    K = 5)

all_went_smote_data <- all_went_smote$data

all_went_smote_data <- all_went_smote_data %>%
    rename(safe_out = class)

all_went_smote_data$safe_out <- as.numeric(all_went_smote_data$safe_out)

all_went_smote_data <- as.data.frame(all_went_smote_data)

smote_glm_model <- glm(safe_out ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = all_went_smote_data, family = binomial)

all_final_data_4$smote_glm_prob <- predict(smote_glm_model, newdata = all_final_data_4, type = "response")

summary(smote_glm_model)
new_glm_model <- glm(safe_out ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = all_went_data, family = binomial)

all_went_data$new_glm_prob <- predict(new_glm_model, type = "response")

sum(ifelse(all_went_data$new_glm_prob > 0, all_went_data$safe_out, 0))
sum(ifelse(all_went_data$new_glm_prob > 0, all_went_data$new_glm_prob, 0))

> sum(ifelse(all_went_data$new_glm_prob > 0, all_went_data$safe_out, 0))
[1] 527
> sum(ifelse(all_went_data$new_glm_prob > 0, all_went_data$new_glm_prob, 0))
[1] 527

> sum(ifelse(all_went_data$new_glm_prob < 0.9, all_went_data$safe_out, 0))
[1] 73
> sum(ifelse(all_went_data$new_glm_prob < 0.9, all_went_data$new_glm_prob, 0))
[1] 72.88007

> sum(ifelse(all_went_data$new_glm_prob < 0.7, all_went_data$safe_out, 0))
[1] 16
> sum(ifelse(all_went_data$new_glm_prob < 0.7, all_went_data$new_glm_prob, 0))
[1] 15.3988

> sum(ifelse(all_went_data$new_glm_prob < 0.5, all_went_data$safe_out, 0))
[1] 3
> sum(ifelse(all_went_data$new_glm_prob < 0.5, all_went_data$new_glm_prob, 0))
[1] 1.894777
try_glm_model <- glm(ifelse(all_went_data$score_chance < 1, 0, 1) ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = all_went_data, family = binomial)

all_final_data_4$try_glm_prob <- predict(try_glm_model, newdata = all_final_data_4, type = "response")

summary(try_glm_model)
all_went_data <- all_went_data[,1:9] %>%
    mutate(def_safe = ifelse(score_chance < 1, 0, 1))
ds_gam_results <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam <- gam(def_safe ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  ds_gam_results <- bind_rows(ds_gam_results, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(ds_gam_results$logloss)
sd(ds_gam_results$logloss)

> mean(ds_gam_results$logloss)
[1] 0.1557032
> sd(ds_gam_results$logloss)
[1] 0.002372976
ds_gam_results_2 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data_2$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam <- gam(def_safe ~ s(run_dist) + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  ds_gam_results_2 <- bind_rows(ds_gam_results_2, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(ds_gam_results_2$logloss)
sd(ds_gam_results_2$logloss)

> mean(ds_gam_results_2$logloss)
[1] 0.1576292
> sd(ds_gam_results_2$logloss)
[1] 0.003358983
ds_gam_results_3 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam <- gam(def_safe ~ run_dist + s(OF_dist) + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  ds_gam_results_3 <- bind_rows(ds_gam_results_3, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(ds_gam_results_3$logloss)
sd(ds_gam_results_3$logloss)

> mean(ds_gam_results_3$logloss)
[1] 0.1558588
> sd(ds_gam_results_3$logloss)
[1] 0.002302427
ds_gam_results_4 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam <- gam(def_safe ~ run_dist + OF_dist + s(run_speed) + top_speed + OF_momentum_home + OF_momentum_side, data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  ds_gam_results_4 <- bind_rows(ds_gam_results_4, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(ds_gam_results_4$logloss)
sd(ds_gam_results_4$logloss)

> mean(ds_gam_results_4$logloss)
[1] 0.1766857
> sd(ds_gam_results_4$logloss)
[1] 0.007814122
ds_gam_results_5 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam <- gam(def_safe ~ run_dist + OF_dist + run_speed + s(top_speed) + OF_momentum_home + OF_momentum_side, data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  ds_gam_results_5 <- bind_rows(ds_gam_results_5, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(ds_gam_results_5$logloss)
sd(ds_gam_results_5$logloss)

> mean(ds_gam_results_5$logloss)
[1] 0.1565778
> sd(ds_gam_results_5$logloss)
[1] 0.002314232
ds_gam_results_6 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam <- gam(def_safe ~ run_dist + OF_dist + run_speed + top_speed + s(OF_momentum_home) + OF_momentum_side, data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  ds_gam_results_6 <- bind_rows(ds_gam_results_6, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(ds_gam_results_6$logloss)
sd(ds_gam_results_6$logloss)

> mean(ds_gam_results_6$logloss)
[1] 0.1562374
> sd(ds_gam_results_6$logloss)
[1] 0.003356306
ds_gam_results_7 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam <- gam(def_safe ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + s(OF_momentum_side), data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  ds_gam_results_7 <- bind_rows(ds_gam_results_7, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(ds_gam_results_7$logloss)
sd(ds_gam_results_7$logloss)

> mean(ds_gam_results_7$logloss)
[1] 0.1561013
> sd(ds_gam_results_7$logloss)
[1] 0.002446433
ds_gam_results_8 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam <- gam(def_safe ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side + (run_dist:OF_dist), data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  ds_gam_results_8 <- bind_rows(ds_gam_results_8, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(ds_gam_results_8$logloss)
sd(ds_gam_results_8$logloss)

> mean(ds_gam_results_8$logloss)
[1] 0.1564697
> sd(ds_gam_results_8$logloss)
[1] 0.002149885
ds_gam_results_9 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam <- gam(def_safe ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side + ti(run_dist, OF_dist), data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  ds_gam_results_9 <- bind_rows(ds_gam_results_9, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(ds_gam_results_9$logloss)
sd(ds_gam_results_9$logloss)

> mean(ds_gam_results_9$logloss)
[1] 0.1628725
> sd(ds_gam_results_9$logloss)
[1] 0.003755322
ds_gam_results_10 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam <- gam(def_safe ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side + (run_speed:top_speed), data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  ds_gam_results_10 <- bind_rows(ds_gam_results_10, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(ds_gam_results_10$logloss)
sd(ds_gam_results_10$logloss)

> mean(ds_gam_results_10$logloss)
[1] 0.1580067
> sd(ds_gam_results_10$logloss)
[1] 0.002773877
ds_gam_results_11 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam <- gam(def_safe ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side + ti(run_speed, top_speed), data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  ds_gam_results_11 <- bind_rows(ds_gam_results_11, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(ds_gam_results_11$logloss)
sd(ds_gam_results_11$logloss)

> mean(ds_gam_results_11$logloss)
[1] 0.1607214
> sd(ds_gam_results_11$logloss)
[1] 0.005421879
ds_gam_results_12 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam <- gam(def_safe ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side + (run_dist:run_speed:top_speed), data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  ds_gam_results_12 <- bind_rows(ds_gam_results_12, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(ds_gam_results_12$logloss)
sd(ds_gam_results_12$logloss)

> mean(ds_gam_results_12$logloss)
[1] 0.1545407
> sd(ds_gam_results_12$logloss)
[1] 0.00234596
ds_gam_results_13 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam <- gam(def_safe ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side + (OF_dist:OF_momentum_home), data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  ds_gam_results_13 <- bind_rows(ds_gam_results_13, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(ds_gam_results_13$logloss)
sd(ds_gam_results_13$logloss)

> mean(ds_gam_results_13$logloss)
[1] 0.1568676
> sd(ds_gam_results_13$logloss)
[1] 0.002627631
ds_gam_results_14 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam <- gam(def_safe ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side + (OF_momentum_home:OF_momentum_side), data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  ds_gam_results_14 <- bind_rows(ds_gam_results_14, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(ds_gam_results_14$logloss)
sd(ds_gam_results_14$logloss)

> mean(ds_gam_results_14$logloss)
[1] 0.1578575
> sd(ds_gam_results_14$logloss)
[1] 0.002249428
ds_gam_results_15 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam <- gam(def_safe ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side + (OF_dist:OF_momentum_home:OF_momentum_side), data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  ds_gam_results_15 <- bind_rows(ds_gam_results_15, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(ds_gam_results_15$logloss)
sd(ds_gam_results_15$logloss)

> mean(ds_gam_results_15$logloss)
[1] 0.1581677
> sd(ds_gam_results_15$logloss)
[1] 0.00253089
ds_gam_results_16 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam <- gam(def_safe ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side + (run_dist:run_speed), data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  ds_gam_results_16 <- bind_rows(ds_gam_results_16, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(ds_gam_results_16$logloss)
sd(ds_gam_results_16$logloss)

> mean(ds_gam_results_16$logloss)
[1] 0.1558783
> sd(ds_gam_results_16$logloss)
[1] 0.002284116
ds_gam_results_17 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam <- gam(def_safe ~ run_dist + s(OF_dist) + run_speed + top_speed + OF_momentum_home + OF_momentum_side + (run_dist:run_speed:top_speed), data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  ds_gam_results_17 <- bind_rows(ds_gam_results_17, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(ds_gam_results_17$logloss)
sd(ds_gam_results_17$logloss)

> mean(ds_gam_results_17$logloss)
[1] 0.1550032
> sd(ds_gam_results_17$logloss)
[1] 0.002426066
ds_gam_results_18 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam <- gam(def_safe ~ s(run_dist) + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side + (run_dist:run_speed:top_speed), data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  ds_gam_results_18 <- bind_rows(ds_gam_results_18, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(ds_gam_results_18$logloss)
sd(ds_gam_results_18$logloss)

> mean(ds_gam_results_18$logloss)
[1] 0.1525796
> sd(ds_gam_results_18$logloss)
[1] 0.002865385
ds_gam_results_19 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam <- gam(def_safe ~ run_dist + OF_dist + s(run_speed) + top_speed + OF_momentum_home + OF_momentum_side + (run_dist:run_speed:top_speed), data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  ds_gam_results_19 <- bind_rows(ds_gam_results_19, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(ds_gam_results_19$logloss)
sd(ds_gam_results_19$logloss)

> mean(ds_gam_results_19$logloss)
[1] 0.1727093
> sd(ds_gam_results_19$logloss)
[1] 0.006809436
ds_gam_results_20 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam <- gam(def_safe ~ run_dist + OF_dist + run_speed + s(top_speed) + OF_momentum_home + OF_momentum_side + (run_dist:run_speed:top_speed), data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  ds_gam_results_20 <- bind_rows(ds_gam_results_20, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(ds_gam_results_20$logloss)
sd(ds_gam_results_20$logloss)

> mean(ds_gam_results_21$logloss)
[1] 0.1601373
> sd(ds_gam_results_21$logloss)
[1] 0.002646925
ds_gam_results_21 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam <- gam(def_safe ~ run_dist + OF_dist + (run_speed * top_speed) + (OF_momentum_home * OF_momentum_side), data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  ds_gam_results_21 <- bind_rows(ds_gam_results_21, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(ds_gam_results_21$logloss)
sd(ds_gam_results_21$logloss)

> mean(ds_gam_results_21$logloss)
[1] 0.1601373
> sd(ds_gam_results_21$logloss)
[1] 0.002646925
ds_gam_results_22 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 5, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam <- gam(def_safe ~ run_dist + OF_dist + (run_speed * top_speed) + (OF_momentum_home * OF_momentum_side), data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  ds_gam_results_22 <- bind_rows(ds_gam_results_22, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(ds_gam_results_22$logloss)
sd(ds_gam_results_22$logloss)

> mean(ds_gam_results_22$logloss)
[1] 0.161732
> sd(ds_gam_results_22$logloss)
[1] 0.004687543
ds_gam_results_23 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 5, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam <- gam(def_safe ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  ds_gam_results_23 <- bind_rows(ds_gam_results_23, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(ds_gam_results_23$logloss)
sd(ds_gam_results_23$logloss)

> mean(ds_gam_results_23$logloss)
[1] 0.1576742
> sd(ds_gam_results_23$logloss)
[1] 0.004162598
ds_gam_results_24 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 5, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam <- gam(def_safe ~ (run_dist * run_speed * top_speed) + (OF_dist * OF_momentum_home * OF_momentum_side), data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  ds_gam_results_24 <- bind_rows(ds_gam_results_24, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(ds_gam_results_24$logloss)
sd(ds_gam_results_24$logloss)

> mean(ds_gam_results_24$logloss)
[1] 0.1732043
> sd(ds_gam_results_24$logloss)
[1] 0.008872286
ds_gam_results_25 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 5, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam <- gam(def_safe ~ s(run_dist) + s(OF_dist) + (run_speed * top_speed) + (OF_momentum_home * OF_momentum_side), data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  ds_gam_results_25 <- bind_rows(ds_gam_results_25, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(ds_gam_results_25$logloss)
sd(ds_gam_results_25$logloss)

> mean(ds_gam_results_25$logloss)
[1] 0.1624453
> sd(ds_gam_results_25$logloss)
[1] 0.00506908
pca_speed <- prcomp(all_went_data[, c("run_speed", "top_speed")], scale. = TRUE)
all_went_data$pca_speed <- pca_speed$x[, 1]

ds_gam_results_26 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 5, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam <- gam(def_safe ~ run_dist + OF_dist + pca_speed + OF_momentum_home + OF_momentum_side, data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  ds_gam_results_26 <- bind_rows(ds_gam_results_26, data.frame(test_num = i, logloss = avg_log_losses))
}

> mean(ds_gam_results_26$logloss)
[1] 0.1555125
> sd(ds_gam_results_26$logloss)
[1] 0.003159275
model_1 <- gam(def_safe ~ s(run_dist) + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side + (run_dist:run_speed:top_speed), data = all_went_data, family = binomial)

all_final_data_4$model_1 <- predict(model_1, newdata = all_final_data_4, type = "response")

model_2 <- glm(def_safe ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side + (run_dist:run_speed:top_speed), data = all_went_data, family = binomial)

all_final_data_4$model_2 <- predict(model_2, newdata = all_final_data_4, type = "response")

model_3 <- glm(def_safe ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = all_went_data, family = binomial)

all_final_data_4$model_3 <- predict(model_3, newdata = all_final_data_4, type = "response")

model_4 <- glm(def_safe ~ run_dist + OF_dist + (run_speed * top_speed) + OF_momentum_home + OF_momentum_side, data = all_went_data, family = binomial)

all_final_data_4$model_4 <- predict(model_4, newdata = all_final_data_4, type = "response")

pca_speed_2 <- prcomp(all_final_data_4[, c("run_speed", "top_speed")], scale. = TRUE)
all_final_data_4$pca_speed <- pca_speed_2$x[, 1]

model_5 <- glm(def_safe ~ run_dist + OF_dist + pca_speed + OF_momentum_home + OF_momentum_side, data = all_went_data, family = binomial)

all_final_data_4$model_5 <- predict(model_5, newdata = all_final_data_4, type = "response")

> sum(all_final_data_4$model_1)
[1] 638.0165
> sum(all_final_data_4$model_2)
[1] 631.8912
> sum(all_final_data_4$model_3)
[1] 621.8182
> sum(all_final_data_4$model_4)
[1] 615.5593

> log_loss(all_final_data_4$went_home, all_final_data_4$model_1)
[1] 0.2471596
> log_loss(all_final_data_4$went_home, all_final_data_4$model_2)
[1] 0.2285383
> log_loss(all_final_data_4$went_home, all_final_data_4$model_3)
[1] 0.2094208
> log_loss(all_final_data_4$went_home, all_final_data_4$model_4)
[1] 0.2017596
final_glm_model <- glm(def_safe ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = all_went_data, family = binomial)

all_final_data_4$glm_safe <- predict(final_glm_model, newdata = all_final_data_4, type = "response")

summary(final_glm_model)

Call:
glm(formula = def_safe ~ run_dist + OF_dist + run_speed + top_speed + 
    OF_momentum_home + OF_momentum_side, family = binomial, data = all_went_data)

Coefficients:
                  Estimate Std. Error z value Pr(>|z|)    
(Intercept)       1.024544   3.602427   0.284    0.776    
run_dist         -0.116052   0.016665  -6.964 3.32e-12 ***
OF_dist           0.034834   0.007685   4.533 5.82e-06 ***
run_speed         0.126953   0.079794   1.591    0.112    
top_speed         0.014632   0.129784   0.113    0.910    
OF_momentum_home -0.069041   0.028378  -2.433    0.015 *  
OF_momentum_side  0.052232   0.035028   1.491    0.136    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 318.22  on 560  degrees of freedom
Residual deviance: 205.26  on 554  degrees of freedom
AIC: 219.26

Number of Fisher Scoring iterations: 8
all_final_data_4 <- all_final_data_4[,c(-24)] %>%
    relocate(glm_safe, .after = prob_to_go)

all_final_data_4 <- all_final_data_4 %>%
    mutate(went_home = ifelse(!is.na(safe_out), 1, 0)) %>%
    mutate(should_go = ifelse(glm_safe >= prob_to_go, 1, 0)) %>%
    mutate(correct_decision = ifelse(went_home == should_go, 1, 0))

mean(all_final_data_4$correct_decision)
[1] 0.9071295

confusion_matrix <- table(Predicted = all_final_data_4$should_go, Actual = all_final_data_4$went_home)
print(confusion_matrix)
         Actual
Predicted   0   1
        0 439  33
        1  66 528

A lot more “should go but didn’t”, than “shouldn’t go but did”
glm_go_model <- glm(went_home ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side + prob_to_go, data = all_final_data_4, family = binomial)

all_final_data_4$glm_go <- predict(glm_go_model, type = "response")

summary(glm_go_model)

Call:
glm(formula = went_home ~ run_dist + OF_dist + run_speed + top_speed + 
    OF_momentum_home + OF_momentum_side + prob_to_go, family = binomial, 
    data = all_final_data_4)

Coefficients:
                   Estimate Std. Error z value Pr(>|z|)    
(Intercept)      -13.243179   2.899038  -4.568 4.92e-06 ***
run_dist          -0.173841   0.014579 -11.924  < 2e-16 ***
OF_dist            0.058135   0.006607   8.798  < 2e-16 ***
run_speed          0.119567   0.058577   2.041   0.0412 *  
top_speed          0.489382   0.085610   5.716 1.09e-08 ***
OF_momentum_home  -0.056046   0.023459  -2.389   0.0169 *  
OF_momentum_side   0.049830   0.027145   1.836   0.0664 .  
prob_to_go         1.358820   1.507281   0.902   0.3673    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 1474.85  on 1065  degrees of freedom
Residual deviance:  328.27  on 1058  degrees of freedom
AIC: 344.27

Number of Fisher Scoring iterations: 8
aw2_gam_results <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data_2$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data_2[fold, ]

    gam <- gam(def_safe ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = all_went_data_2[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  aw2_gam_results <- bind_rows(aw2_gam_results, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(aw2_gam_results$logloss)
sd(aw2_gam_results$logloss)

> mean(aw2_gam_results$logloss)
[1] 0.1511781
> sd(aw2_gam_results$logloss)
[1] 0.001998859
aw2_gam_results_2 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data_2$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data_2[fold, ]

    gam <- gam(def_safe ~ run_dist + OF_dist + (run_speed * top_speed) + OF_momentum_home + OF_momentum_side, data = all_went_data_2[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  aw2_gam_results_2 <- bind_rows(aw2_gam_results_2, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(aw2_gam_results_2$logloss)
sd(aw2_gam_results_2$logloss)

> mean(aw2_gam_results_2$logloss)
[1] 0.1539735
> sd(aw2_gam_results_2$logloss)
[1] 0.002270609
aw2_gam_results_3 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data_2$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data_2[fold, ]

    gam <- gam(def_safe ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side + (run_dist:run_speed:top_speed), data = all_went_data_2[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  aw2_gam_results_3 <- bind_rows(aw2_gam_results_3, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(aw2_gam_results_3$logloss)
sd(aw2_gam_results_3$logloss)

> mean(aw2_gam_results_3$logloss)
[1] 0.1510877
> sd(aw2_gam_results_3$logloss)
[1] 0.002176812
aw2_gam_results_4 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data_2$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data_2[fold, ]

    gam <- gam(def_safe ~ s(run_dist) + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side + (run_dist:run_speed:top_speed), data = all_went_data_2[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  aw2_gam_results_4 <- bind_rows(aw2_gam_results_4, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(aw2_gam_results_4$logloss)
sd(aw2_gam_results_4$logloss)

> mean(aw2_gam_results_4$logloss)
[1] 0.1512372
> sd(aw2_gam_results_4$logloss)
[1] 0.002384087
aw2_lda_results <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data_2$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data_2[fold, ]

    lda <- lda(all_went_data_2[-fold, 2:7], grouping = all_went_data_2[-fold, ]$def_safe)

    test_probs <- predict(lda, test_data[, 2:7])$posterior[, 2]

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  aw2_lda_results <- bind_rows(aw2_lda_results, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(aw2_lda_results$logloss)
sd(aw2_lda_results$logloss)

> mean(aw2_lda_results$logloss)
[1] 0.1680457
> sd(aw2_lda_results$logloss)
[1] 0.002307353
aw2_qda_results <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data_2$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data_2[fold, ]

    qda <- qda(all_went_data_2[-fold, 2:7], grouping = all_went_data_2[-fold, ]$def_safe)

    test_probs <- predict(qda, test_data[, 2:7])$posterior[, 2]

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  aw2_qda_results <- bind_rows(aw2_qda_results, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(aw2_qda_results$logloss)
sd(aw2_qda_results$logloss)

> mean(aw2_qda_results$logloss)
[1] 0.2620757
> sd(aw2_qda_results$logloss)
[1] 0.02060027
aw2_rf_results <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data_2$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data_2[fold, ]

    rf <- randomForest(as.factor(def_safe) ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = all_went_data_2[-fold, ], ntree = 100)

    test_probs <- predict(rf, newdata = test_data, type = "prob")[, 2]

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  aw2_rf_results <- bind_rows(aw2_rf_results, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(aw2_rf_results$logloss)
sd(aw2_rf_results$logloss)

> mean(aw2_rf_results$logloss)
[1] 0.1836057
> sd(aw2_rf_results$logloss)
[1] 0.01580887
aw2_rf_results_2 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data_2$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data_2[fold, ]

    rf <- randomForest(as.factor(def_safe) ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = all_went_data_2[-fold, ], ntree = 250)

    test_probs <- predict(rf, newdata = test_data, type = "prob")[, 2]

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  aw2_rf_results_2 <- bind_rows(aw2_rf_results_2, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(aw2_rf_results_2$logloss)
sd(aw2_rf_results_2$logloss)

> mean(aw2_rf_results_2$logloss)
[1] 0.1775654
> sd(aw2_rf_results_2$logloss)
[1] 0.0110374
aw2_rf_results_3 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data_2$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data_2[fold, ]

    rf <- randomForest(as.factor(def_safe) ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = all_went_data_2[-fold, ], ntree = 500)

    test_probs <- predict(rf, newdata = test_data, type = "prob")[, 2]

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  aw2_rf_results_3 <- bind_rows(aw2_rf_results_3, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(aw2_rf_results_3$logloss)
sd(aw2_rf_results_3$logloss)

> mean(aw2_rf_results_3$logloss)
[1] 0.1770727
> sd(aw2_rf_results_3$logloss)
[1] 0.007046146
final_glm_model <- glm(def_safe ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = all_went_data_2, family = binomial)

all_final_data_5$glm_safe <- predict(final_glm_model, newdata = all_final_data_5, type = "response")

summary(final_glm_model)

Call:
glm(formula = def_safe ~ run_dist + OF_dist + run_speed + top_speed + 
    OF_momentum_home + OF_momentum_side, family = binomial, data = all_went_data_2)

Coefficients:
                  Estimate Std. Error z value Pr(>|z|)    
(Intercept)      -0.007934   3.663865  -0.002   0.9983    
run_dist         -0.126707   0.017885  -7.085 1.39e-12 ***
OF_dist           0.038564   0.008053   4.789 1.68e-06 ***
run_speed         0.127474   0.080853   1.577   0.1149    
top_speed         0.053205   0.131786   0.404   0.6864    
OF_momentum_home -0.067346   0.029042  -2.319   0.0204 *  
OF_momentum_side  0.053349   0.035253   1.513   0.1302    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 318.05  on 559  degrees of freedom
Residual deviance: 198.92  on 553  degrees of freedom
AIC: 212.92

Number of Fisher Scoring iterations: 8
all_final_data_5 <- all_final_data_5[,c(-24)] %>%
    relocate(glm_safe, .after = prob_to_go)

all_final_data_5 <- all_final_data_5 %>%
    mutate(went_home = ifelse(!is.na(safe_out), 1, 0)) %>%
    mutate(should_go = ifelse(glm_safe >= prob_to_go, 1, 0)) %>%
    mutate(correct_decision = ifelse(went_home == should_go, 1, 0))

mean(all_final_data_5$correct_decision)
[1] 0.913615

confusion_matrix <- table(Predicted = all_final_data_5$should_go, Actual = all_final_data_5$went_home)
print(confusion_matrix)
        Actual
Predicted   0   1
        0 449  36
        1  56 524

Didn’t send 9.7% of players who the model would send
Sent 7.4% of players the model wouldn’t send
glm_go_model <- glm(went_home ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side + prob_to_go, data = all_final_data_5, family = binomial)

all_final_data_5$glm_go <- predict(glm_go_model, type = "response")

summary(glm_go_model)

Call:
glm(formula = went_home ~ run_dist + OF_dist + run_speed + top_speed + 
    OF_momentum_home + OF_momentum_side + prob_to_go, family = binomial, 
    data = all_final_data_5)

Coefficients:
                   Estimate Std. Error z value Pr(>|z|)    
(Intercept)      -13.854990   2.983893  -4.643 3.43e-06 ***
run_dist          -0.181263   0.015445 -11.736  < 2e-16 ***
OF_dist            0.060903   0.006902   8.824  < 2e-16 ***
run_speed          0.124659   0.059246   2.104   0.0354 *  
top_speed          0.506380   0.087866   5.763 8.26e-09 ***
OF_momentum_home  -0.054247   0.024017  -2.259   0.0239 *  
OF_momentum_side   0.051663   0.027690   1.866   0.0621 .  
prob_to_go         1.444180   1.546060   0.934   0.3503    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 1473.56  on 1064  degrees of freedom
Residual deviance:  314.87  on 1057  degrees of freedom
AIC: 330.87

Number of Fisher Scoring iterations: 8
all_final_data_5 <- all_final_data_5 %>%
    mutate(safe_min_go = glm_safe - glm_go)
So + equals too conservative, - equals too aggressive

safe_min_go_glm <- glm(safe_min_go ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side + prob_to_go, data = all_final_data_5)

summary(safe_min_go_glm)
Call:
glm(formula = safe_min_go ~ run_dist + OF_dist + run_speed + 
    top_speed + OF_momentum_home + OF_momentum_side + prob_to_go, 
    data = all_final_data_5)

Coefficients:
                   Estimate Std. Error t value Pr(>|t|)    
(Intercept)       5.498e-01  4.538e-02  12.118  < 2e-16 ***
run_dist          4.294e-04  7.651e-05   5.613 2.54e-08 ***
OF_dist          -3.684e-04  9.311e-05  -3.957 8.10e-05 ***
run_speed         8.292e-03  1.083e-03   7.654 4.37e-14 ***
top_speed        -2.343e-02  1.570e-03 -14.923  < 2e-16 ***
OF_momentum_home -1.787e-03  4.228e-04  -4.227 2.57e-05 ***
OF_momentum_side  1.246e-05  5.477e-04   0.023    0.982    
prob_to_go       -1.276e-02  2.868e-02  -0.445    0.656    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for gaussian family taken to be 0.008743824)

    Null deviance: 11.7175  on 1064  degrees of freedom
Residual deviance:  9.2422  on 1057  degrees of freedom
AIC: -2015.2

Number of Fisher Scoring iterations: 2

Think this means… 
Too conservative with high run_dist / too aggressive with low run_dist
Too conservative with low OF_dist / too aggressive with high OF_dist
Too conservative with high run_speed / too aggressive with low run_speed
Too conservative with low top_speed / too aggressive with high top_speed
Too conservative with low OF_momentum_home / too aggressive with high OF_momentum_home
team_info <- full_join(game_info_2[,c(1, 16, 24)], game_events_2[,c(2, 12)], by = "ppg_key")

team_info <- team_info %>%
    mutate(play_key = paste(game_str, play_id, sep = "_")) %>%
    mutate(team = substr(batter, start = 1, stop = 3))

team_info <- team_info %>%
    distinct(play_key, .keep_all = TRUE)

all_final_data_6 <- left_join(all_final_data_5, team_info[,5:6], by = "play_key")

ACN AKX ALA APZ AVV BEJ BVD CGA CMS CRQ DMS DYE ESW FBP GAX GEA GHD GIS HCI HKR HMN IAK IAQ IHI 
  3   4   2   9   3   2   4   5   2   7   5  34   7  12   6   4   7   9   3  10   7   1   9   3 
IKJ IXC JFU JIL JJS JMJ JNJ JZK KIR KJH KNB KQQ LFS MCJ MGJ MHY MRJ NSO NYA OWH OXG PHS POW PTB 
 11   3   7   7   6   8   1   5   4   5  20   6   7   2   4  13  11   2   4  11  29   1   3   5 
PVJ QEA QZE RDO RQJ RZQ STK TEQ TKJ TSZ UEX UKI UPU UWE VHS VMN VZE WJU WMI WNA WZR XAX XFE XLB 
  6  71  11   5   5 217  12   2   5  11   7   6  10   2   5   3   8   9   6   6   5   2   9   2 
YJD YUH 
192   3

all_final_data_6 <- all_final_data_6 %>%
    mutate(RE_diff = ((glm_safe * go_safe_RE) + ((1 - glm_safe) * go_out_RE)) - stay_RE )

mean(abs(ifelse(all_final_data_6$correct_decision == 0, abs(all_final_data_6$RE_diff), 0)))
[1] 0.01653246

QEA_final_data <- all_final_data_6 %>%
    filter(team == "QEA")
QEA_confusion_matrix <- table(Predicted = QEA_final_data$should_go, Actual = QEA_final_data$went_home)
print(QEA_confusion_matrix)
        Actual
Predicted  0  1
        0 20  5
        1  6 40
Didn’t send 13.0% of players who the model would send
Sent 20% of players the model wouldn’t send

mean(abs(ifelse(QEA_final_data$correct_decision == 0, abs(QEA_final_data$RE_diff), 0)))
[1] 0.01545368


RZQ_final_data <- all_final_data_6 %>%
    filter(team == "RZQ")
RZQ_confusion_matrix <- table(Predicted = RZQ_final_data$should_go, Actual = RZQ_final_data$went_home)
print(RZQ_confusion_matrix)
        Actual
Predicted   0   1
        0  95   6
        1   9 107

Didn’t send 7.8% of players who the model would send
Sent 5.9% of players the model wouldn’t send

mean(abs(ifelse(RZQ_final_data$correct_decision == 0, abs(RZQ_final_data$RE_diff), 0)))
[1] 0.01477362


YJD_final_data <- all_final_data_6 %>%
    filter(team == "YJD")
YJD_confusion_matrix <- table(Predicted = YJD_final_data$should_go, Actual = YJD_final_data$went_home)
print(YJD_confusion_matrix)
        Actual
Predicted  0  1
        0 77  6
        1 12 97

Didn’t send 11% of players who the model would send
Sent 7.2% of players the model wouldn’t send

mean(abs(ifelse(YJD_final_data$correct_decision == 0, abs(YJD_final_data$RE_diff), 0)))
[1] 0.01508011
all_final_data_scaled <- all_final_data_6

all_final_data_scaled[,c(2:7,23)] <- scale(all_final_data_6[,c(2:7,23)])

scaled_should_go <- glm(should_go ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side + prob_to_go, data = all_final_data_scaled, family = binomial)

coef(scaled_should_go)
    (Intercept)         run_dist          OF_dist        run_speed        top_speed 
        91.77071       -607.70968        229.73608         63.50010         15.78930 
OF_momentum_home OF_momentum_side       prob_to_go 
       -93.98406         34.02201        -65.48629

sd(coef(scaled_should_go))
[1] 249.3603

coef(scaled_should_go) / sd(coef(scaled_should_go))
     (Intercept)         run_dist          OF_dist        run_speed        top_speed 
      0.36802447      -2.43707432       0.92130159       0.25465196       0.06331921 
OF_momentum_home OF_momentum_side       prob_to_go 
     -0.37690058       0.13643715      -0.26261709


scaled_went <- glm(went_home ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side + prob_to_go, data = all_final_data_scaled, family = binomial)

coef(scaled_went)
    (Intercept)         run_dist          OF_dist        run_speed        top_speed 
       0.5945561       -6.8326076        2.8662002        0.4321529        1.2093262 
OF_momentum_home OF_momentum_side       prob_to_go 
      -0.5698051        0.2780468        0.1450003

sd(coef(scaled_went))
[1] 2.850687

coef(scaled_went) / sd(coef(scaled_went))
     (Intercept)         run_dist          OF_dist        run_speed        top_speed 
      0.20856591      -2.39682843       1.00544192       0.15159605       0.42422273 
OF_momentum_home OF_momentum_side       prob_to_go 
     -0.19988342       0.09753675       0.05086505

Third base coaches according to model…
Slightly don’t use OF_dist enough
Slightly don’t use run_speed enough
Use top_speed too much
Don’t use OF_momentum_home enough
Slightly don’t use OF_momentum_side enough
Don’t use prob_to_go / situation nearly enough



save.image("SMT_data.RData")
write.csv(stats, "stats.csv", row.names = FALSE)

How to Implement XGBoost in R

library(tidyverse)
library(arrow)
library(sportyR)
library(mgcv)
library(purrr)
library(progressr)
library(baseballr)
library(xgboost)
library(caret)
library(pROC)
library(ParBayesianOptimization)
library(randomForest)
library(MASS)
library(caret)
library(e1071)
library(ROSE)
library(smotefamily)
library(patchwork)

See percent of time batter also takes the extra base when the runner is going

Do a 70/30, 80/20, and 90/10 random training test splits to see what is best

Possible new info to get:
Runners at what bases situation (need for run expectency)
Inning (need)
Outs (could assume 1 out like Billy, or could estimate based on point in inning)
Score (could estimate with thing talked abt in discord with runs being guys who batted - runners left on base - 3 or wtv it was)
Next batter up in order

Stratified safe out split
Split safe out data
Take 80% safe and 80% out

MiLB Run-Expectancy Matrix. I converted Minor League play-by-play… | by Sam Wirth | Dev Genius

Del y1_d007_JJS_YJD_179 from 1



random_grid <- tibble(
  alpha = runif(100, 0, 10),
  lambda = runif(100, 0, 10),
  early_stopping_rounds = sample(1:20, 100, replace = TRUE),
  max_depth = sample(1:5, 100, replace = TRUE),
  subsample = runif(100, 0.5, 1),
  colsample_bytree = runif(100, 0.5, 1),
  min_child_weight = sample(1:10, 100, replace = TRUE),
  gamma = runif(100, 0, 10)
)

results <- data.frame()

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data_2$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  params <- list(
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = "logloss",
    max_depth = random_grid$max_depth[i],
    subsample = random_grid$subsample[i],
    colsample_bytree = random_grid$colsample_bytree[i],
    min_child_weight = random_grid$min_child_weight[i],
    gamma = random_grid$gamma[i],
    alpha = random_grid$alpha[i],
    lambda = random_grid$lambda[i]
  )

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data_2[fold, ]

  xgb <- xgboost(
    params = params,
    data = as.matrix(all_went_data_2[-fold, 2:7]),
    label = all_went_data_2[-fold, ]$def_safe,
    nrounds = 1000,
    eta = 0.05,
    early_stopping_rounds = random_grid$early_stopping_rounds[i],
    verbose = 0
  )

    test_probs <- predict(xgb, newdata = as.matrix(test_data[,2:7]), type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  results <- bind_rows(results, cbind(random_grid[i, ], logloss = avg_log_losses))
}

g1 <- ggplot(results, aes(x = nrounds, y = logloss)) + geom_point() + labs(title = "nrounds")
g2 <- ggplot(results, aes(x = eta, y = logloss)) + geom_point() + labs(title = "eta")
g3 <- ggplot(results, aes(x = early_stopping_rounds, y = logloss)) + geom_point() + labs(title = "early_stopping_rounds")
g4 <- ggplot(results, aes(x = max_depth, y = logloss)) + geom_point() + labs(title = "max_depth")
g5 <- ggplot(results, aes(x = subsample, y = logloss)) + geom_point() + labs(title = "subsample")
g6 <- ggplot(results, aes(x = colsample_bytree, y = logloss)) + geom_point() + labs(title = "colsample_bytree")
g7 <- ggplot(results, aes(x = min_child_weight, y = logloss)) + geom_point() + labs(title = "min_child_weight")
g8 <- ggplot(results, aes(x = gamma, y = logloss)) + geom_point() + labs(title = "gamma")

(g1 | g2 | g3) / (g4 | g5 | g6) / (g7 | g8)

cor(results$alpha, results$logloss)
cor(results$lambda, results$logloss)
cor(results$early_stopping_rounds, results$logloss)
cor(results$max_depth, results$logloss)
cor(results$subsample, results$logloss)
cor(results$colsample_bytree, results$logloss)
cor(results$min_child_weight, results$logloss)
cor(results$gamma, results$logloss)

> cor(results$alpha, results$logloss)
[1] 0.708789
> cor(results$lambda, results$logloss)
[1] 0.1661774
> cor(results$early_stopping_rounds, results$logloss)
[1] -0.1434663
> cor(results$max_depth, results$logloss)
[1] 0.129238
> cor(results$subsample, results$logloss)
[1] -0.3116824
> cor(results$colsample_bytree, results$logloss)
[1] 0.002614411
> cor(results$min_child_weight, results$logloss)
[1] 0.08476638
> cor(results$gamma, results$logloss)
[1] 0.5973746

ggplot(results, aes(x = alpha, y = early_stopping_rounds, color = logloss)) +
  geom_point() +                # Add points
  scale_color_gradient(low = "green", high = "red")






gam_xgb <- gam(logloss ~ te(alpha, gamma), data = results)

results$proj_logloss <- predict(gam_xgb)

results <- results %>%
    mutate(proj_diff = proj_logloss - logloss)

cor(results$alpha, results$proj_diff)
cor(results$lambda, results$proj_diff)
cor(results$early_stopping_rounds, results$proj_diff)
cor(results$max_depth, results$proj_diff)
cor(results$subsample, results$proj_diff)
cor(results$colsample_bytree, results$proj_diff)
cor(results$min_child_weight, results$proj_diff)
cor(results$gamma, results$proj_diff)

> cor(results$alpha, results$proj_diff)
[1] 2.331842e-15
> cor(results$lambda, results$proj_diff)
[1] -0.2352112
> cor(results$early_stopping_rounds, results$proj_diff)
[1] 0.4378366
> cor(results$max_depth, results$proj_diff)
[1] 0.09361635
> cor(results$subsample, results$proj_diff)
[1] 0.5729901
> cor(results$colsample_bytree, results$proj_diff)
[1] 0.07533655
> cor(results$min_child_weight, results$proj_diff)
[1] -0.2962544
> cor(results$gamma, results$proj_diff)
[1] -6.084246e-16

ggplot(results, aes(x = subsample, y = proj_logloss)) +
  geom_point()
random_grid <- tibble(
  nrounds = sample(20:1000, 100, replace = TRUE),
  early_stopping_rounds = sample(1:20, 100, replace = TRUE),
  eta = runif(100, 0.01, 0.5),
)

results <- data.frame()

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data_2$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  params <- list(
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = "logloss",
    eta = random_grid$eta[i]
  )

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data_2[fold, ]

  xgb <- xgboost(
    params = params,
    data = as.matrix(all_went_data_2[-fold, 2:7]),
    label = all_went_data_2[-fold, ]$def_safe,
    nrounds = random_grid$nrounds[i],
    early_stopping_rounds = random_grid$early_stopping_rounds[i],
    verbose = 0
  )

    test_probs <- predict(xgb, newdata = as.matrix(test_data[,2:7]), type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  results <- bind_rows(results, cbind(random_grid[i, ], logloss = avg_log_losses))
}

cor(results$nrounds, results$logloss)
cor(results$eta, results$logloss)
cor(results$early_stopping_rounds, results$logloss)

> cor(results$nrounds, results$logloss)
[1] 0.5393186
> cor(results$eta, results$logloss)
[1] 0.6368334
> cor(results$early_stopping_rounds, results$logloss)
[1] -0.0002065106

ggplot(results, aes(x = nrounds, y = eta, color = logloss)) + geom_point() + scale_color_gradient(low = "green", high = "red")

random_grid <- tibble(
  nrounds = sample(10:500, 100, replace = TRUE),
  early_stopping_rounds = sample(1:20, 100, replace = TRUE),
  eta = runif(100, 0.001, 0.2),
)

results <- data.frame()

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data_2$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  params <- list(
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = "logloss",
    eta = random_grid$eta[i]
  )

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data_2[fold, ]

  xgb <- xgboost(
    params = params,
    data = as.matrix(all_went_data_2[-fold, 2:7]),
    label = all_went_data_2[-fold, ]$def_safe,
    nrounds = random_grid$nrounds[i],
    early_stopping_rounds = random_grid$early_stopping_rounds[i],
    verbose = 0
  )

    test_probs <- predict(xgb, newdata = as.matrix(test_data[,2:7]), type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  results <- bind_rows(results, cbind(random_grid[i, ], logloss = avg_log_losses))
}

cor(results$nrounds, results$logloss)
cor(results$eta, results$logloss)
cor(results$early_stopping_rounds, results$logloss)

> cor(results$nrounds, results$logloss)
[1] -0.2638771
> cor(results$eta, results$logloss)
[1] -0.2928961
> cor(results$early_stopping_rounds, results$logloss)
[1] 0.1013682

ggplot(results %>% filter(logloss < 0.3), aes(x = nrounds, y = eta, color = logloss)) + geom_point() + scale_color_gradient(low = "green", high = "red")

random_grid <- tibble(
  max_depth = sample(1:5, 100, replace = TRUE),
  min_child_weight =  = runif(100, 0, 10),
  gamma = runif(100, 0, 10),
  alpha = runif(100, 0, 10),
  lambda = runif(100, 0, 10)
)

results <- data.frame()

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data_2$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  params <- list(
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = "logloss",
    max_depth = random_grid$max_depth[i],
    min_child_weight = random_grid$min_child_weight[i],
    gamma = random_grid$gamma[i],
    alpha = random_grid$alpha[i],
    lambda = random_grid$lambda[i]
  )

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data_2[fold, ]

  xgb <- xgboost(
    params = params,
    data = as.matrix(all_went_data_2[-fold, 2:7]),
    label = all_went_data_2[-fold, ]$def_safe,
    nrounds = 225,
    eta = 0.03,
    verbose = 0
  )

    test_probs <- predict(xgb, newdata = as.matrix(test_data[,2:7]), type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  results <- bind_rows(results, cbind(random_grid[i, ], logloss = avg_log_losses))
}

cor(results$max_depth, results$logloss)
cor(results$min_child_weight, results$logloss)
cor(results$gamma, results$logloss)
cor(results$alpha, results$logloss)
cor(results$lambda, results$logloss)

> cor(results$max_depth, results$logloss)
[1] -0.2866324
> cor(results$min_child_weight, results$logloss)
[1] -0.09806205
> cor(results$gamma, results$logloss)
[1] 0.6222763
> cor(results$alpha, results$logloss)
[1] 0.5121492
> cor(results$lambda, results$logloss)
[1] 0.1175086


ggplot(results), aes(x = gamma, y = alpha, color = logloss)) + geom_point() + scale_color_gradient(low = "green", high = "red")

random_grid <- tibble(
  max_depth = sample(1:5, 100, replace = TRUE),
  min_child_weight = runif(100, 0, 10)
)

results <- data.frame()

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data_2$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  params <- list(
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = "logloss",
    max_depth = random_grid$max_depth[i],
    min_child_weight = random_grid$min_child_weight[i],
    gamma = 0,
    alpha = 0
  )

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data_2[fold, ]

  xgb <- xgboost(
    params = params,
    data = as.matrix(all_went_data_2[-fold, 2:7]),
    label = all_went_data_2[-fold, ]$def_safe,
    nrounds = 225,
    eta = 0.03,
    verbose = 0
  )

    test_probs <- predict(xgb, newdata = as.matrix(test_data[,2:7]), type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  results <- bind_rows(results, cbind(random_grid[i, ], logloss = avg_log_losses))
}

cor(results$max_depth, results$logloss)
cor(results$min_child_weight, results$logloss)

> cor(results$max_depth, results$logloss)
[1] 0.09833154
> cor(results$min_child_weight, results$logloss)
[1] -0.1587021

ggplot(results, aes(x = max_depth, y = min_child_weight, color = logloss)) + geom_point() + scale_color_gradient(low = "green", high = "red")

ggplot(results, aes(x = max_depth, y = min_child_weight, color = logloss)) + geom_point() + scale_color_gradient(low = "green", high = "red")

ggplot(results %>% filter(logloss < 0.18), aes(x = max_depth, y = min_child_weight, color = logloss)) + geom_point() + scale_color_gradient(low = "green", high = "red")



random_grid <- tibble(
  subsample = runif(100, 0.5, 1),
  colsample_bytree = runif(100, 0.5, 1)
)

results <- data.frame()

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data_2$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  params <- list(
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = "logloss",
    subsample = random_grid$subsample[i],
    colsample_bytree = random_grid$colsample_bytree[i]
  )

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data_2[fold, ]

  xgb <- xgboost(
    params = params,
    data = as.matrix(all_went_data_2[-fold, 2:7]),
    label = all_went_data_2[-fold, ]$def_safe,
    nrounds = 225,
    eta = 0.03,
    max_depth = 2,
    min_child_weight = 4,
    verbose = 0
  )

    test_probs <- predict(xgb, newdata = as.matrix(test_data[,2:7]), type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  results <- bind_rows(results, cbind(random_grid[i, ], logloss = avg_log_losses))
}

cor(results$subsample, results$logloss)
cor(results$colsample_bytree, results$logloss)
> cor(results$subsample, results$logloss)
[1] -0.2047935
> cor(results$colsample_bytree, results$logloss)
[1] -0.4987488

ggplot(results, aes(x = subsample, y = colsample_bytree, color = logloss)) + geom_point() + scale_color_gradient(low = "green", high = "red")

xgb_results <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data_2$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data_2[fold, ]

  xgb <- xgboost(
    data = as.matrix(all_went_data_2[-fold, ][,2:7]),
    label = all_went_data_2[-fold, ]$def_safe,
    objective = "binary:logistic",
    eval_metric = "logloss",
    nrounds = 225,
    eta = 0.03,
    max_depth = 2,
    min_child_weight = 4,
    subsample = 0.85,
    colsample_bytree = 0.9
  )

    test_probs <- predict(xgb, newdata = as.matrix(test_data[,2:7]), type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  xgb_results <- bind_rows(xgb_results, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(xgb_results$logloss)
sd(xgb_results$logloss)

> mean(xgb_results$logloss)
[1] 0.1683589
> sd(xgb_results$logloss)
[1] 0.002628913
test_xgb <- xgboost(
    data = as.matrix(all_went_data_2[,2:7]),
    label = all_went_data_2$def_safe,
    objective = "binary:logistic",
    eval_metric = "logloss",
    nrounds = 156,
    eta = 0.341,
    early_stopping_rounds = 13,
    max_depth = 2,
    min_child_weight = 3,
    subsample = 0.822,
    gamma = 0.536,
    verbose = 0,
  )

    all_final_data_5$xgb <- predict(test_xgb, newdata = as.matrix(all_final_data_5[,2:7]), type = "response")

Still not working on that 95 / 278.5 example where speeds are different




nvBayes_results <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    nvBayes <- naiveBayes(def_safe ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = all_went_data[-fold, ])

    test_probs <- predict(nvBayes, newdata = test_data, type = "raw")[, "1"]

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  nvBayes_results <- bind_rows(nvBayes_results, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(nvBayes_results$logloss)
sd(nvBayes_results$logloss)

> mean(nvBayes_results$logloss)
[1] 0.2061259
> sd(nvBayes_results$logloss)
[1] 0.008316792

