library(dplyr)

# Using final decided on model
final_glm_model <- glm(def_safe ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = all_went_data_2, family = binomial)

# Predicting on all data with model
all_final_data_5$glm_safe <- predict(final_glm_model, newdata = all_final_data_5, type = "response")

all_final_data_5 <- all_final_data_5[,c(-24)] %>%
    relocate(glm_safe, .after = prob_to_go)

# Scaling data to get comparable coefficient values for variables used in model
all_went_data_scaled <- all_went_data_2

all_went_data_scaled[,2:7] <- scale(all_went_data_scaled[,2:7])

scaled_final_model <- glm(def_safe ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = all_went_data_scaled, family = binomial)

coef(scaled_final_model)
#     (Intercept)         run_dist          OF_dist        run_speed        top_speed 
#        5.0617794       -3.6093396        1.9650246        0.3470706        0.1083703 
# OF_momentum_home OF_momentum_side 
#       -0.7371754        0.2937933

exp(coef(final_glm_model))
#      (Intercept)         run_dist          OF_dist        run_speed        top_speed 
#        0.9920972        0.8809916        1.0393174        1.1359548        1.0546463 
# OF_momentum_home OF_momentum_side 
#        0.9348720        1.0547978

# Using these odd differences, we can calculate what the new probability would be if each variable went up by one unit from a 50/50 safe or out play
exp(coef(final_glm_model)["run_dist"]) / (1 + exp(coef(final_glm_model)["run_dist"]))
#  run_dist 
# 0.4683655 

exp(coef(final_glm_model)["OF_dist"]) / (1 + exp(coef(final_glm_model)["OF_dist"]))
#   OF_dist 
# 0.5096398

exp(coef(final_glm_model)["run_speed"]) / (1 + exp(coef(final_glm_model)["run_speed"]))
# run_speed 
# 0.5318253

exp(coef(final_glm_model)["top_speed"]) / (1 + exp(coef(final_glm_model)["top_speed"]))
# top_speed 
# 0.5132982

exp(coef(final_glm_model)["OF_momentum_home"]) / (1 + exp(coef(final_glm_model)["OF_momentum_home"]))
# OF_momentum_home 
#        0.4831699

exp(coef(final_glm_model)["OF_momentum_side"]) / (1 + exp(coef(final_glm_model)["OF_momentum_side"]))
# OF_momentum_side 
#        0.5133341


# Seeing if they should've gone based off prob_to_gp
all_final_data_5 <- all_final_data_5 %>%
    mutate(went_home = ifelse(!is.na(safe_out), 1, 0)) %>%
    mutate(should_go = ifelse(glm_safe >= prob_to_go, 1, 0)) %>%
    mutate(correct_decision = ifelse(went_home == should_go, 1, 0))

mean(all_final_data_5$correct_decision)
# 0.913615

confusion_matrix <- table(Predicted = all_final_data_5$should_go, Actual = all_final_data_5$went_home)
print(confusion_matrix)
#         Actual
# Predicted   0   1
#         0 449  36
#         1  56 524

# Seeing how each third base coach did for all the major 3 teams used in dataset
team_info <- team_info %>%
    mutate(play_key = paste(game_str, play_id, sep = "_")) %>%
    mutate(team = substr(batter, start = 1, stop = 3))

team_info <- team_info %>%
    distinct(play_key, .keep_all = TRUE)

all_final_data_6 <- left_join(all_final_data_5, team_info[,5:6], by = "play_key")

# ACN AKX ALA APZ AVV BEJ BVD CGA CMS CRQ DMS DYE ESW FBP GAX GEA GHD GIS HCI HKR HMN IAK IAQ IHI 
#   3   4   2   9   3   2   4   5   2   7   5  34   7  12   6   4   7   9   3  10   7   1   9   3 
# IKJ IXC JFU JIL JJS JMJ JNJ JZK KIR KJH KNB KQQ LFS MCJ MGJ MHY MRJ NSO NYA OWH OXG PHS POW PTB 
#  11   3   7   7   6   8   1   5   4   5  20   6   7   2   4  13  11   2   4  11  29   1   3   5 
# PVJ QEA QZE RDO RQJ RZQ STK TEQ TKJ TSZ UEX UKI UPU UWE VHS VMN VZE WJU WMI WNA WZR XAX XFE XLB 
#   6  71  11   5   5 217  12   2   5  11   7   6  10   2   5   3   8   9   6   6   5   2   9   2 
# YJD YUH 
# 192   3

all_final_data_6 <- all_final_data_6 %>%
    mutate(RE_diff = ((glm_safe * go_safe_RE) + ((1 - glm_safe) * go_out_RE)) - stay_RE )

# average run expectancy lost per decision
mean(abs(ifelse(all_final_data_6$correct_decision == 0, abs(all_final_data_6$RE_diff), 0)))
# 0.01653246

100 * mean(abs(ifelse(all_final_data_6$correct_decision == 0, abs(all_final_data_6$RE_diff), 0)))
# 1.653246


# average run expectancy lost per decision where they should send a runner home
sum(abs(ifelse(all_final_data_6$correct_decision == 0 & all_final_data_6$should_go == 1, abs(all_final_data_6$RE_diff), 0))) / sum(ifelse(all_final_data_6$should_go == 1, 1, 0))
#  0.0179932

100 * sum(abs(ifelse(all_final_data_6$correct_decision == 0 & all_final_data_6$should_go == 1, abs(all_final_data_6$RE_diff), 0))) / sum(ifelse(all_final_data_6$should_go == 1, 1, 0))
# 1.79932


# average run expectancy lost per decision where they should keep the runner at third
sum(abs(ifelse(all_final_data_6$correct_decision == 0 & all_final_data_6$should_go == 0, abs(all_final_data_6$RE_diff), 0))) / sum(ifelse(all_final_data_6$should_go == 0, 1, 0))
# 0.0147856

100 * sum(abs(ifelse(all_final_data_6$correct_decision == 0 & all_final_data_6$should_go == 0, abs(all_final_data_6$RE_diff), 0))) / sum(ifelse(all_final_data_6$should_go == 0, 1, 0))
# 1.47856

##########################################################################################################################################

QEA_final_data <- all_final_data_6 %>%
    filter(team == "QEA")
QEA_confusion_matrix <- table(Predicted = QEA_final_data$should_go, Actual = QEA_final_data$went_home)
print(QEA_confusion_matrix)
#         Actual
# Predicted  0  1
#         0 20  5
#         1  6 40

mean(QEA_final_data$correct_decision)
# 0.8450704

100 * mean(abs(ifelse(QEA_final_data$correct_decision == 0, abs(QEA_final_data$RE_diff), 0)))
# 1.545368

100 * sum(abs(ifelse(QEA_final_data$correct_decision == 0 & QEA_final_data$should_go == 1, abs(QEA_final_data$RE_diff), 0))) / sum(ifelse(QEA_final_data$should_go == 1, 1, 0))
# 1.572245

100 * sum(abs(ifelse(QEA_final_data$correct_decision == 0 & QEA_final_data$should_go == 0, abs(QEA_final_data$RE_diff), 0))) / sum(ifelse(QEA_final_data$should_go == 0, 1, 0))
# 1.495914

##########################################################################################################################################

RZQ_final_data <- all_final_data_6 %>%
    filter(team == "RZQ")
RZQ_confusion_matrix <- table(Predicted = RZQ_final_data$should_go, Actual = RZQ_final_data$went_home)
print(RZQ_confusion_matrix)
#         Actual
# Predicted   0   1
#         0  95   6
#         1   9 107

mean(RZQ_final_data$correct_decision)
# 0.9308756

100 * mean(abs(ifelse(RZQ_final_data$correct_decision == 0, abs(RZQ_final_data$RE_diff), 0)))
# 1.477362

100 * sum(abs(ifelse(RZQ_final_data$correct_decision == 0 & RZQ_final_data$should_go == 1, abs(RZQ_final_data$RE_diff), 0))) / sum(ifelse(RZQ_final_data$should_go == 1, 1, 0))
# 1.810925

100 * sum(abs(ifelse(RZQ_final_data$correct_decision == 0 & RZQ_final_data$should_go == 0, abs(RZQ_final_data$RE_diff), 0))) / sum(ifelse(RZQ_final_data$should_go == 0, 1, 0))
# 1.09426

##########################################################################################################################################

YJD_final_data <- all_final_data_6 %>%
    filter(team == "YJD")
YJD_confusion_matrix <- table(Predicted = YJD_final_data$should_go, Actual = YJD_final_data$went_home)
print(YJD_confusion_matrix)
#         Actual
# Predicted  0  1
#         0 77  6
#         1 12 97

mean(YJD_final_data$correct_decision)
# 0.90625

100 * mean(abs(ifelse(YJD_final_data$correct_decision == 0, abs(YJD_final_data$RE_diff), 0)))
# 1.508011

100 * sum(abs(ifelse(YJD_final_data$correct_decision == 0 & YJD_final_data$should_go == 1, abs(YJD_final_data$RE_diff), 0))) / sum(ifelse(YJD_final_data$should_go == 1, 1, 0))
# 1.924894

100 * sum(abs(ifelse(YJD_final_data$correct_decision == 0 & YJD_final_data$should_go == 0, abs(YJD_final_data$RE_diff), 0))) / sum(ifelse(YJD_final_data$should_go == 0, 1, 0))
# 0.9605384

##########################################################################################################################################

# Getting coefficients, have to scale all the variabkles to get comparable coefficients
all_final_data_scaled <- all_final_data_6

all_final_data_scaled[,c(2:7,23)] <- scale(all_final_data_6[,c(2:7,23)])

scaled_should_go <- glm(should_go ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side + prob_to_go, data = all_final_data_scaled, family = binomial)

coef(scaled_should_go)
#     (Intercept)         run_dist          OF_dist        run_speed        top_speed 
#         91.77071       -607.70968        229.73608         63.50010         15.78930 
# OF_momentum_home OF_momentum_side       prob_to_go 
#        -93.98406         34.02201        -65.48629

sd(coef(scaled_should_go))
# 249.3603

# Need to divide by standard deviation to be able to compare both models
coef(scaled_should_go) / sd(coef(scaled_should_go))
#      (Intercept)         run_dist          OF_dist        run_speed        top_speed 
#       0.36802447      -2.43707432       0.92130159       0.25465196       0.06331921 
# OF_momentum_home OF_momentum_side       prob_to_go 
#      -0.37690058       0.13643715      -0.26261709


scaled_went <- glm(went_home ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side + prob_to_go, data = all_final_data_scaled, family = binomial)

coef(scaled_went)
#     (Intercept)         run_dist          OF_dist        run_speed        top_speed 
#        0.5945561       -6.8326076        2.8662002        0.4321529        1.2093262 
# OF_momentum_home OF_momentum_side       prob_to_go 
#       -0.5698051        0.2780468        0.1450003

sd(coef(scaled_went))
# 2.850687

coef(scaled_went) / sd(coef(scaled_went))
#      (Intercept)         run_dist          OF_dist        run_speed        top_speed 
#       0.20856591      -2.39682843       1.00544192       0.15159605       0.42422273 
# OF_momentum_home OF_momentum_side       prob_to_go 
#      -0.19988342       0.09753675       0.05086505
