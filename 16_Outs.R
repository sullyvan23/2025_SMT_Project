library(dplyr)

game_info_2 <- game_info %>%
    mutate(ppg_key = paste(game_str, play_per_game, sep = "_"))

game_events_2 <- game_events %>%
    mutate(ppg_key = paste(game_str, play_per_game, sep = "_"))

# Joining data by game and play_per_game so I can see when a play_id happened in an inning
out_info <- full_join(game_info_2[,c(1,5:6,24)], game_events_2[,c(2,12)], by = "ppg_key")

out_info <- out_info %>%
    distinct(ppg_key, .keep_all = TRUE)

out_info <- out_info %>%
    filter(!is.na(game_str))

write.csv(out_info, "out_info.csv", row.names = FALSE)
# Reloaded from Excel with half_inning and out data

# sum(out_info$outs)
# 71803
# Same as number of rows, so average of 1 out, good

out_info <- out_info %>%
    mutate(play_key = paste(game_str, play_id, sep = "_"))

all_final_data_4 <- left_join(all_final_data_3, out_info[,7:8], by = "play_key")

all_final_data_4 <- all_final_data_4 %>%
    mutate(outs = ifelse(is.na(outs), 1, outs))

sum(all_final_data_4$outs)
# 1323
# Average of about 1.235 outs, makes sense since runners on base likely later in inning
