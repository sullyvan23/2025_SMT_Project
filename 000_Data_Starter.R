# Reading in data into R, provided by Billy Fryer in the discord, thank you

library(arrow)
library(tidyverse)

game_info <- arrow::open_csv_dataset(paste0(data_directory,"/game_info"), 
                                     partitioning = c("HomeTeam", "AwayTeam", "Season", "Day"), 
                                     hive_style = F, 
                                     unify_schemas = T, 
                                     na = c("", "NA", "NULL", NA, "\\N"))

ball_pos <- arrow::open_csv_dataset(paste0(data_directory,"/ball_pos"), 
                                    partitioning = c("HomeTeam", "AwayTeam", "Season", "Day"), 
                                    col_names = c("game_str", "play_id", "timestamp",
                                                  "ball_position_x", "ball_position_y", "ball_position_z"),
                                    hive_style = F, 
                                    unify_schemas = T, 
                                    na = c("", "NA", "NULL", NA, "\\N"))

game_events <- arrow::open_csv_dataset(paste0(data_directory,"/game_events"), 
                                       partitioning = c("HomeTeam", "AwayTeam", "Season", "Day"),  
                                       hive_style = F, 
                                       unify_schemas = T, 
                                       na = c("", "NA", "NULL", NA, "\\N"))

player_pos <- arrow::open_csv_dataset(paste0(data_directory,"/player_pos"), 
                                      partitioning = c("HomeTeam", "AwayTeam", "Season", "Day"), 
                                      col_names = c("game_str", "play_id", "timestamp",
                                                    "player_position", "field_x", "field_y"),
                                      hive_style = F, 
                                      unify_schemas = T, 
                                      na = c("", "NA", "NULL", NA, "\\N"))
