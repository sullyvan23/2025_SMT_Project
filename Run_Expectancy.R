install.packages("baseballr")
library(baseballr)

library(purrr)
library(progressr)

handlers(global = TRUE)
handlers("txtprogressbar")

schedule <- mlb_schedule(season = 2021,
    level_ids = c(11:14))

game_ids <- unique(schedule$game_pk)

# Getting all MiLB game data from 2021 with progress bar so I could actually see how long it was taking (it took a while)
with_progress({
    p <- progressor(along = game_ids)
    
    pbp <- map(game_ids, ~{
        p()
        daily_stats(.x)
    }) %>%
        bind_rows()
})

# Selecting necessary columns
pbp_data <- pbp %>%
    dplyr::select(game_pk, atBatIndex, result.awayScore, result.homeScore, count.outs.start, about.inning, about.halfInning, matchup.postOnFirst.fullName, matchup.postOnSecond.fullName, matchup.postOnThird.fullName)

pbp_data <- pbp_data %>%
    mutate(inning_key = paste(game_pk, about.halfInning, about.inning, sep = "_"))

pbp_data_ordered <- pbp_data %>%
    arrange(inning_key, atBatIndex)


# Getting run information needed
pbp_data_ordered <- pbp_data_ordered %>%
    mutate(current_runs = ifelse(about.halfInning == "top", result.awayScore, result.homeScore))

pbp_data_ordered <- pbp_data_ordered %>%
    group_by(inning_key) %>%
    mutate(end_half_runs = max(current_runs))

pbp_data_ordered <- pbp_data_ordered %>%
    mutate(runs_yet_to_score = end_half_runs - current_runs)

pbp_data_ordered <- pbp_data_ordered %>%
    distinct(inning_key, atBatIndex, .keep_all = TRUE)


# Getting baserunner and out data
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

# Getting run expectancies
RE_avgs <- pbp_data_ordered %>%
    group_by(situation) %>%
    summarize(RE = mean(runs_yet_to_score))

run_expectancy <- left_join(run_expectancy, RE_avgs, by = "situation")

run_expectancy <- run_expectancy %>%
    mutate(run_exp = ifelse(is.na(RE), 0, RE))

run_expectancy <- run_expectancy[,c(1,3)]
