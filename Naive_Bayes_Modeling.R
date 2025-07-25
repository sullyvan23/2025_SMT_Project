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
