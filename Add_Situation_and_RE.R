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
    mutate(def_safe = ifelse(score_chance < 1, 0, 1))
