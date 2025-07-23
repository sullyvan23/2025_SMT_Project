messy_stats_1st <- messy_stats_1st %>%
    mutate(run_dist = ifelse( (run_x >= 0 & run_y > 90/sqrt(2)) , 
        (180 + sqrt( (run_x)^2 + (run_y - (90*sqrt(2)))^2 ) ) ,
        ifelse( (run_x < 0 & run_y > 90/sqrt(2)) , 
            (90 + sqrt( (run_x + (90/sqrt(2)))^2 + (run_y - (90/sqrt(2)))^2 ) ) , 
            ifelse( (run_x > 0 | run_y < 0), 0, sqrt(run_x^2 + run_y^2) ) ) 
    ) )

messy_stats_1st <- messy_stats_1st %>%
    mutate(OF_dist = sqrt(OF_x^2 + OF_y^2))
