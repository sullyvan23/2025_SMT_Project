library(dplyr)

# Testing bunch of different combinations with logistic regression model
ds_gam_results <- data.frame(test_num = integer(), logloss = numeric())
# Doiung 100 tests for each model
for (i in 1:100) {
  # Creating 10 statified folds of went data based off def_safe
  strat_folds <- createFolds(all_went_data$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  # looping through all combinations of 9 train folds and 1 test fold
  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]
    # Model used for test
    gam <- gam(def_safe ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = all_went_data[-fold, ], family = binomial)
    # Finding log loss between predicted probability and score_chance
    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)
  # Putting logloss of tests into a dataframe
  ds_gam_results <- bind_rows(ds_gam_results, data.frame(test_num = i, logloss = avg_log_losses))
}
# Finding mean and sd logloss on 100 tests
mean(ds_gam_results$logloss)
sd(ds_gam_results$logloss)

# mean(ds_gam_results$logloss)
# 0.1557032
# sd(ds_gam_results$logloss)
# 0.002372976

################################################################################################################################################################

ds_gam_results_2 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam <- gam(def_safe ~ s(run_dist) + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  ds_gam_results_2 <- bind_rows(ds_gam_results_2, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(ds_gam_results_2$logloss)
sd(ds_gam_results_2$logloss)

# mean(ds_gam_results_2$logloss)
# 0.1576292
# sd(ds_gam_results_2$logloss)
# 0.003358983

################################################################################################################################################################

ds_gam_results_3 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam <- gam(def_safe ~ run_dist + s(OF_dist) + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  ds_gam_results_3 <- bind_rows(ds_gam_results_3, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(ds_gam_results_3$logloss)
sd(ds_gam_results_3$logloss)

# mean(ds_gam_results_3$logloss)
# 0.1558588
# sd(ds_gam_results_3$logloss)
# 0.002302427

################################################################################################################################################################

ds_gam_results_4 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam <- gam(def_safe ~ run_dist + OF_dist + s(run_speed) + top_speed + OF_momentum_home + OF_momentum_side, data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  ds_gam_results_4 <- bind_rows(ds_gam_results_4, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(ds_gam_results_4$logloss)
sd(ds_gam_results_4$logloss)

# mean(ds_gam_results_4$logloss)
# 0.1766857
# sd(ds_gam_results_4$logloss)
# 0.007814122

################################################################################################################################################################

ds_gam_results_5 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam <- gam(def_safe ~ run_dist + OF_dist + run_speed + s(top_speed) + OF_momentum_home + OF_momentum_side, data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  ds_gam_results_5 <- bind_rows(ds_gam_results_5, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(ds_gam_results_5$logloss)
sd(ds_gam_results_5$logloss)

# mean(ds_gam_results_5$logloss)
# 0.1565778
# sd(ds_gam_results_5$logloss)
# 0.002314232

################################################################################################################################################################

ds_gam_results_6 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam <- gam(def_safe ~ run_dist + OF_dist + run_speed + top_speed + s(OF_momentum_home) + OF_momentum_side, data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  ds_gam_results_6 <- bind_rows(ds_gam_results_6, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(ds_gam_results_6$logloss)
sd(ds_gam_results_6$logloss)

# mean(ds_gam_results_6$logloss)
# 0.1562374
# sd(ds_gam_results_6$logloss)
# 0.003356306

################################################################################################################################################################

ds_gam_results_7 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam <- gam(def_safe ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + s(OF_momentum_side), data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  ds_gam_results_7 <- bind_rows(ds_gam_results_7, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(ds_gam_results_7$logloss)
sd(ds_gam_results_7$logloss)

# mean(ds_gam_results_7$logloss)
# 0.1561013
# sd(ds_gam_results_7$logloss)
# 0.002446433

################################################################################################################################################################

ds_gam_results_8 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam <- gam(def_safe ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side + (run_dist:OF_dist), data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  ds_gam_results_8 <- bind_rows(ds_gam_results_8, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(ds_gam_results_8$logloss)
sd(ds_gam_results_8$logloss)

# mean(ds_gam_results_8$logloss)
# 0.1564697
# sd(ds_gam_results_8$logloss)
# 0.002149885

################################################################################################################################################################

ds_gam_results_9 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam <- gam(def_safe ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side + ti(run_dist, OF_dist), data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  ds_gam_results_9 <- bind_rows(ds_gam_results_9, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(ds_gam_results_9$logloss)
sd(ds_gam_results_9$logloss)

# mean(ds_gam_results_9$logloss)
# 0.1628725
# sd(ds_gam_results_9$logloss)
# 0.003755322

################################################################################################################################################################

ds_gam_results_10 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam <- gam(def_safe ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side + (run_speed:top_speed), data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  ds_gam_results_10 <- bind_rows(ds_gam_results_10, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(ds_gam_results_10$logloss)
sd(ds_gam_results_10$logloss)

# mean(ds_gam_results_10$logloss)
# 0.1580067
# sd(ds_gam_results_10$logloss)
# 0.002773877

################################################################################################################################################################

ds_gam_results_11 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam <- gam(def_safe ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side + ti(run_speed, top_speed), data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  ds_gam_results_11 <- bind_rows(ds_gam_results_11, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(ds_gam_results_11$logloss)
sd(ds_gam_results_11$logloss)

# mean(ds_gam_results_11$logloss)
# 0.1607214
# sd(ds_gam_results_11$logloss)
# 0.005421879

################################################################################################################################################################

ds_gam_results_12 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam <- gam(def_safe ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side + (run_dist:run_speed:top_speed), data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  ds_gam_results_12 <- bind_rows(ds_gam_results_12, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(ds_gam_results_12$logloss)
sd(ds_gam_results_12$logloss)

# mean(ds_gam_results_12$logloss)
# 0.1545407
# sd(ds_gam_results_12$logloss)
# 0.00234596

################################################################################################################################################################

ds_gam_results_13 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam <- gam(def_safe ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side + (OF_dist:OF_momentum_home), data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  ds_gam_results_13 <- bind_rows(ds_gam_results_13, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(ds_gam_results_13$logloss)
sd(ds_gam_results_13$logloss)

# mean(ds_gam_results_13$logloss)
# 0.1568676
# sd(ds_gam_results_13$logloss)
# 0.002627631

################################################################################################################################################################

ds_gam_results_14 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam <- gam(def_safe ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side + (OF_momentum_home:OF_momentum_side), data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  ds_gam_results_14 <- bind_rows(ds_gam_results_14, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(ds_gam_results_14$logloss)
sd(ds_gam_results_14$logloss)

# mean(ds_gam_results_14$logloss)
# 0.1578575
# sd(ds_gam_results_14$logloss)
# 0.002249428

################################################################################################################################################################

ds_gam_results_15 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam <- gam(def_safe ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side + (OF_dist:OF_momentum_home:OF_momentum_side), data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  ds_gam_results_15 <- bind_rows(ds_gam_results_15, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(ds_gam_results_15$logloss)
sd(ds_gam_results_15$logloss)

# mean(ds_gam_results_15$logloss)
# 0.1581677
# sd(ds_gam_results_15$logloss)
# 0.00253089

################################################################################################################################################################

ds_gam_results_16 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam <- gam(def_safe ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side + (run_dist:run_speed), data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  ds_gam_results_16 <- bind_rows(ds_gam_results_16, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(ds_gam_results_16$logloss)
sd(ds_gam_results_16$logloss)

# mean(ds_gam_results_16$logloss)
# 0.1558783
# sd(ds_gam_results_16$logloss)
# 0.002284116

################################################################################################################################################################

ds_gam_results_17 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam <- gam(def_safe ~ run_dist + s(OF_dist) + run_speed + top_speed + OF_momentum_home + OF_momentum_side + (run_dist:run_speed:top_speed), data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  ds_gam_results_17 <- bind_rows(ds_gam_results_17, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(ds_gam_results_17$logloss)
sd(ds_gam_results_17$logloss)

# mean(ds_gam_results_17$logloss)
# 0.1550032
# sd(ds_gam_results_17$logloss)
# 0.002426066

################################################################################################################################################################

ds_gam_results_18 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam <- gam(def_safe ~ s(run_dist) + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side + (run_dist:run_speed:top_speed), data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  ds_gam_results_18 <- bind_rows(ds_gam_results_18, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(ds_gam_results_18$logloss)
sd(ds_gam_results_18$logloss)

# mean(ds_gam_results_18$logloss)
# 0.1525796
# sd(ds_gam_results_18$logloss)
# 0.002865385

################################################################################################################################################################

ds_gam_results_19 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam <- gam(def_safe ~ run_dist + OF_dist + s(run_speed) + top_speed + OF_momentum_home + OF_momentum_side + (run_dist:run_speed:top_speed), data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  ds_gam_results_19 <- bind_rows(ds_gam_results_19, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(ds_gam_results_19$logloss)
sd(ds_gam_results_19$logloss)

# mean(ds_gam_results_19$logloss)
# 0.1727093
# sd(ds_gam_results_19$logloss)
# 0.006809436

################################################################################################################################################################

ds_gam_results_20 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam <- gam(def_safe ~ run_dist + OF_dist + run_speed + s(top_speed) + OF_momentum_home + OF_momentum_side + (run_dist:run_speed:top_speed), data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  ds_gam_results_20 <- bind_rows(ds_gam_results_20, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(ds_gam_results_20$logloss)
sd(ds_gam_results_20$logloss)

# mean(ds_gam_results_21$logloss)
# 0.1601373
# sd(ds_gam_results_21$logloss)
# 0.002646925

################################################################################################################################################################

ds_gam_results_21 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam <- gam(def_safe ~ run_dist + OF_dist + (run_speed * top_speed) + (OF_momentum_home * OF_momentum_side), data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  ds_gam_results_21 <- bind_rows(ds_gam_results_21, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(ds_gam_results_21$logloss)
sd(ds_gam_results_21$logloss)

# mean(ds_gam_results_21$logloss)
# 0.1601373
# sd(ds_gam_results_21$logloss)
# 0.002646925

################################################################################################################################################################

ds_gam_results_22 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 5, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam <- gam(def_safe ~ run_dist + OF_dist + (run_speed * top_speed) + (OF_momentum_home * OF_momentum_side), data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  ds_gam_results_22 <- bind_rows(ds_gam_results_22, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(ds_gam_results_22$logloss)
sd(ds_gam_results_22$logloss)

# mean(ds_gam_results_22$logloss)
# 0.161732
# sd(ds_gam_results_22$logloss)
# 0.004687543

################################################################################################################################################################

ds_gam_results_23 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 5, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam <- gam(def_safe ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  ds_gam_results_23 <- bind_rows(ds_gam_results_23, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(ds_gam_results_23$logloss)
sd(ds_gam_results_23$logloss)

# mean(ds_gam_results_23$logloss)
# 0.1576742
# sd(ds_gam_results_23$logloss)
# 0.004162598

################################################################################################################################################################

ds_gam_results_24 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 5, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam <- gam(def_safe ~ (run_dist * run_speed * top_speed) + (OF_dist * OF_momentum_home * OF_momentum_side), data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  ds_gam_results_24 <- bind_rows(ds_gam_results_24, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(ds_gam_results_24$logloss)
sd(ds_gam_results_24$logloss)

# mean(ds_gam_results_24$logloss)
# 0.1732043
# sd(ds_gam_results_24$logloss)
# 0.008872286

################################################################################################################################################################

ds_gam_results_25 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 5, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam <- gam(def_safe ~ s(run_dist) + s(OF_dist) + (run_speed * top_speed) + (OF_momentum_home * OF_momentum_side), data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  ds_gam_results_25 <- bind_rows(ds_gam_results_25, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(ds_gam_results_25$logloss)
sd(ds_gam_results_25$logloss)

# mean(ds_gam_results_25$logloss)
# 0.1624453
# sd(ds_gam_results_25$logloss)
# 0.00506908

################################################################################################################################################################
# Tried something with combining run_speed and top_speed into one stat, didn't really do anything
pca_speed <- prcomp(all_went_data[, c("run_speed", "top_speed")], scale. = TRUE)
all_went_data$pca_speed <- pca_speed$x[, 1]

ds_gam_results_26 <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 5, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

    gam <- gam(def_safe ~ run_dist + OF_dist + pca_speed + OF_momentum_home + OF_momentum_side, data = all_went_data[-fold, ], family = binomial)

    test_probs <- predict(gam, newdata = test_data, type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  ds_gam_results_26 <- bind_rows(ds_gam_results_26, data.frame(test_num = i, logloss = avg_log_losses))
}

# mean(ds_gam_results_26$logloss)
# 0.1555125
# sd(ds_gam_results_26$logloss)
# 0.003159275
