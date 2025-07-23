messy_stats <- messy_stats %>%
    mutate(run_dist = ifelse( (run_x > 0 | run_y < 0) , 
        0 ,
        ifelse( (run_y > 90/sqrt(2)) , 
            (90 + sqrt( (run_x + (90/sqrt(2)))^2 + (run_y - (90/sqrt(2)))^2 ) ) , 
            sqrt(run_x^2 + run_y^2) ) 
    ) )

messy_stats <- messy_stats %>%
    mutate(OF_dist = sqrt(OF_x^2 + OF_y^2))
