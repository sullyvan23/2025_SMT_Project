aw2_lda_results <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data_2$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data_2[fold, ]

    lda <- lda(all_went_data_2[-fold, 2:7], grouping = all_went_data_2[-fold, ]$def_safe)

    test_probs <- predict(lda, test_data[, 2:7])$posterior[, 2]

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  aw2_lda_results <- bind_rows(aw2_lda_results, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(aw2_lda_results$logloss)
sd(aw2_lda_results$logloss)

> mean(aw2_lda_results$logloss)
[1] 0.1680457
> sd(aw2_lda_results$logloss)
[1] 0.002307353
aw2_qda_results <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data_2$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data_2[fold, ]

    qda <- qda(all_went_data_2[-fold, 2:7], grouping = all_went_data_2[-fold, ]$def_safe)

    test_probs <- predict(qda, test_data[, 2:7])$posterior[, 2]

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  aw2_qda_results <- bind_rows(aw2_qda_results, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(aw2_qda_results$logloss)
sd(aw2_qda_results$logloss)

> mean(aw2_qda_results$logloss)
[1] 0.2620757
> sd(aw2_qda_results$logloss)
[1] 0.02060027
