library(dplyr)

messy_stats_1st <- messy_stats_1st %>%
    mutate(run_dist = ifelse( (run_x >= 0 & run_y > 90/sqrt(2)) ,                         # if runner hasn't reached second base yet
        (180 + sqrt( (run_x)^2 + (run_y - (90*sqrt(2)))^2 ) ) ,                           # distance to second base + 180    
        ifelse( (run_x < 0 & run_y > 90/sqrt(2)) ,                                        # if runner hasn't reached third base yet
            (90 + sqrt( (run_x + (90/sqrt(2)))^2 + (run_y - (90/sqrt(2)))^2 ) ) ,         # distance to third base + 90     
            ifelse( (run_x > 0 | run_y < 0), 0, sqrt(run_x^2 + run_y^2) ) )               # else, distance to home
    ) )

messy_stats_1st <- messy_stats_1st %>%
    mutate(OF_dist = sqrt(OF_x^2 + OF_y^2))                    # OF distance to home
