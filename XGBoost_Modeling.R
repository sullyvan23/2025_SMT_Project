# Made random grids of multiple different hyperparameters at a time in hopes of finding the optimal values

random_grid <- tibble(
  nrounds = sample(20:1000, 100, replace = TRUE),
  early_stopping_rounds = sample(1:20, 100, replace = TRUE),
  eta = runif(100, 0.01, 0.5),
)

results <- data.frame()
# 100 tests of different paramaters for variables in random grid above and measure logloss for each to see what is optimal
for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  params <- list(
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = "logloss",
    eta = random_grid$eta[i]
  )

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

  xgb <- xgboost(
    params = params,
    data = as.matrix(all_went_data[-fold, 2:7]),
    label = all_went_data[-fold, ]$def_safe,
    nrounds = random_grid$nrounds[i],
    early_stopping_rounds = random_grid$early_stopping_rounds[i],
    verbose = 0
  )

    test_probs <- predict(xgb, newdata = as.matrix(test_data[,2:7]), type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  results <- bind_rows(results, cbind(random_grid[i, ], logloss = avg_log_losses))
}

cor(results$nrounds, results$logloss)
cor(results$eta, results$logloss)
cor(results$early_stopping_rounds, results$logloss)

> cor(results$nrounds, results$logloss)
[1] 0.5393186
> cor(results$eta, results$logloss)
[1] 0.6368334
> cor(results$early_stopping_rounds, results$logloss)
[1] -0.0002065106

ggplot(results, aes(x = nrounds, y = eta, color = logloss)) + geom_point() + scale_color_gradient(low = "green", high = "red")

#################################################################################################################################################

random_grid <- tibble(
  nrounds = sample(10:500, 100, replace = TRUE),
  early_stopping_rounds = sample(1:20, 100, replace = TRUE),
  eta = runif(100, 0.001, 0.2),
)

results <- data.frame()

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  params <- list(
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = "logloss",
    eta = random_grid$eta[i]
  )

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

  xgb <- xgboost(
    params = params,
    data = as.matrix(all_went_data[-fold, 2:7]),
    label = all_went_data[-fold, ]$def_safe,
    nrounds = random_grid$nrounds[i],
    early_stopping_rounds = random_grid$early_stopping_rounds[i],
    verbose = 0
  )

    test_probs <- predict(xgb, newdata = as.matrix(test_data[,2:7]), type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  results <- bind_rows(results, cbind(random_grid[i, ], logloss = avg_log_losses))
}

cor(results$nrounds, results$logloss)
cor(results$eta, results$logloss)
cor(results$early_stopping_rounds, results$logloss)

> cor(results$nrounds, results$logloss)
[1] -0.2638771
> cor(results$eta, results$logloss)
[1] -0.2928961
> cor(results$early_stopping_rounds, results$logloss)
[1] 0.1013682

ggplot(results %>% filter(logloss < 0.3), aes(x = nrounds, y = eta, color = logloss)) + geom_point() + scale_color_gradient(low = "green", high = "red")

#################################################################################################################################################

random_grid <- tibble(
  max_depth = sample(1:5, 100, replace = TRUE),
  min_child_weight =  = runif(100, 0, 10),
  gamma = runif(100, 0, 10),
  alpha = runif(100, 0, 10),
  lambda = runif(100, 0, 10)
)

results <- data.frame()

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  params <- list(
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = "logloss",
    max_depth = random_grid$max_depth[i],
    min_child_weight = random_grid$min_child_weight[i],
    gamma = random_grid$gamma[i],
    alpha = random_grid$alpha[i],
    lambda = random_grid$lambda[i]
  )

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

  xgb <- xgboost(
    params = params,
    data = as.matrix(all_went_data[-fold, 2:7]),
    label = all_went_data[-fold, ]$def_safe,
    nrounds = 225,
    eta = 0.03,
    verbose = 0
  )

    test_probs <- predict(xgb, newdata = as.matrix(test_data[,2:7]), type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  results <- bind_rows(results, cbind(random_grid[i, ], logloss = avg_log_losses))
}

cor(results$max_depth, results$logloss)
cor(results$min_child_weight, results$logloss)
cor(results$gamma, results$logloss)
cor(results$alpha, results$logloss)
cor(results$lambda, results$logloss)

> cor(results$max_depth, results$logloss)
[1] -0.2866324
> cor(results$min_child_weight, results$logloss)
[1] -0.09806205
> cor(results$gamma, results$logloss)
[1] 0.6222763
> cor(results$alpha, results$logloss)
[1] 0.5121492
> cor(results$lambda, results$logloss)
[1] 0.1175086

ggplot(results), aes(x = gamma, y = alpha, color = logloss)) + geom_point() + scale_color_gradient(low = "green", high = "red")

#################################################################################################################################################

random_grid <- tibble(
  max_depth = sample(1:5, 100, replace = TRUE),
  min_child_weight = runif(100, 0, 10)
)

results <- data.frame()

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  params <- list(
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = "logloss",
    max_depth = random_grid$max_depth[i],
    min_child_weight = random_grid$min_child_weight[i],
    gamma = 0,
    alpha = 0
  )

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

  xgb <- xgboost(
    params = params,
    data = as.matrix(all_went_data[-fold, 2:7]),
    label = all_went_data[-fold, ]$def_safe,
    nrounds = 225,
    eta = 0.03,
    verbose = 0
  )

    test_probs <- predict(xgb, newdata = as.matrix(test_data[,2:7]), type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  results <- bind_rows(results, cbind(random_grid[i, ], logloss = avg_log_losses))
}

cor(results$max_depth, results$logloss)
cor(results$min_child_weight, results$logloss)

> cor(results$max_depth, results$logloss)
[1] 0.09833154
> cor(results$min_child_weight, results$logloss)
[1] -0.1587021

ggplot(results, aes(x = max_depth, y = min_child_weight, color = logloss)) + geom_point() + scale_color_gradient(low = "green", high = "red")

ggplot(results, aes(x = max_depth, y = min_child_weight, color = logloss)) + geom_point() + scale_color_gradient(low = "green", high = "red")

ggplot(results %>% filter(logloss < 0.18), aes(x = max_depth, y = min_child_weight, color = logloss)) + geom_point() + scale_color_gradient(low = "green", high = "red")


#################################################################################################################################################

random_grid <- tibble(
  subsample = runif(100, 0.5, 1),
  colsample_bytree = runif(100, 0.5, 1)
)

results <- data.frame()

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  params <- list(
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = "logloss",
    subsample = random_grid$subsample[i],
    colsample_bytree = random_grid$colsample_bytree[i]
  )

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

  xgb <- xgboost(
    params = params,
    data = as.matrix(all_went_data[-fold, 2:7]),
    label = all_went_data[-fold, ]$def_safe,
    nrounds = 225,
    eta = 0.03,
    max_depth = 2,
    min_child_weight = 4,
    verbose = 0
  )

    test_probs <- predict(xgb, newdata = as.matrix(test_data[,2:7]), type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  results <- bind_rows(results, cbind(random_grid[i, ], logloss = avg_log_losses))
}

cor(results$subsample, results$logloss)
cor(results$colsample_bytree, results$logloss)
> cor(results$subsample, results$logloss)
[1] -0.2047935
> cor(results$colsample_bytree, results$logloss)
[1] -0.4987488

ggplot(results, aes(x = subsample, y = colsample_bytree, color = logloss)) + geom_point() + scale_color_gradient(low = "green", high = "red")

xgb_results <- data.frame(test_num = integer(), logloss = numeric())

for (i in 1:100) {

  strat_folds <- createFolds(all_went_data$def_safe, k = 10, list = TRUE, returnTrain = FALSE)

  tests_log_losses <- c()

  for (fold in strat_folds) {
    test_data <- all_went_data[fold, ]

  xgb <- xgboost(
    data = as.matrix(all_went_data[-fold, ][,2:7]),
    label = all_went_data[-fold, ]$def_safe,
    objective = "binary:logistic",
    eval_metric = "logloss",
    nrounds = 225,
    eta = 0.03,
    max_depth = 2,
    min_child_weight = 4,
    subsample = 0.85,
    colsample_bytree = 0.9
  )

    test_probs <- predict(xgb, newdata = as.matrix(test_data[,2:7]), type = "response")

    fold_log_losses <- log_loss(test_data$score_chance, test_probs)

    tests_log_losses <- c(tests_log_losses, fold_log_losses)
    }

  avg_log_losses <- mean(tests_log_losses)

  xgb_results <- bind_rows(xgb_results, data.frame(test_num = i, logloss = avg_log_losses))
}

mean(xgb_results$logloss)
sd(xgb_results$logloss)

> mean(xgb_results$logloss)
[1] 0.1683589
> sd(xgb_results$logloss)
[1] 0.002628913
