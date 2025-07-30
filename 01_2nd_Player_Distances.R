library(dplyr)

messy_stats <- messy_stats %>%
    mutate(run_dist = ifelse( (run_x > 0 | run_y < 0) ,                                             # if runner has already reached home
        0 ,                                                                                         # distance is 0
        ifelse( (run_y > 90/sqrt(2)) ,                                                              # if runner hasn't reached third base yet
            (90 + sqrt( (run_x + (90/sqrt(2)))^2 + (run_y - (90/sqrt(2)))^2 ) ) ,                   # distance to third base + 90
            sqrt(run_x^2 + run_y^2) )                                                               # else, distance to home
    ) )

messy_stats <- messy_stats %>%
    mutate(OF_dist = sqrt(OF_x^2 + OF_y^2))                                     # OF distance to home
