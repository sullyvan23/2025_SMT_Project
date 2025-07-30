aw2_rf_results <- data.frame(test_num = integer(), logloss = numeric())

# Doing 100 tests of 10-fold stratified cross validation of random forest predicted safe probability vs score_chance
for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    rf <- randomForest(as.factor(def_safe) ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = all_went_data[-fold, ], ntree = 100)

    test_probs <- predict(rf, newdata = test_data, type = "prob")[, 2]

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  aw2_rf_results <- bind_rows(aw2_rf_results, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(aw2_rf_results$logloss)
sd(aw2_rf_results$logloss)

# mean(aw2_rf_results$logloss)
# 0.1836057
# sd(aw2_rf_results$logloss)
# 0.01580887

################################################################################################################################################

# Testing varying number of rounds
aw2_rf_results_2 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    rf <- randomForest(as.factor(def_safe) ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = all_went_data[-fold, ], ntree = 250)

    test_probs <- predict(rf, newdata = test_data, type = "prob")[, 2]

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  aw2_rf_results_2 <- bind_rows(aw2_rf_results_2, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(aw2_rf_results_2$logloss)
sd(aw2_rf_results_2$logloss)

# mean(aw2_rf_results_2$logloss)
# 0.1775654
# sd(aw2_rf_results_2$logloss)
# 0.0110374

################################################################################################################################################

aw2_rf_results_3 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    rf <- randomForest(as.factor(def_safe) ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = all_went_data[-fold, ], ntree = 500)

    test_probs <- predict(rf, newdata = test_data, type = "prob")[, 2]

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  aw2_rf_results_3 <- bind_rows(aw2_rf_results_3, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(aw2_rf_results_3$logloss)
sd(aw2_rf_results_3$logloss)

# mean(aw2_rf_results_3$logloss)
# 0.1770727
# sd(aw2_rf_results_3$logloss)
# 0.007046146
