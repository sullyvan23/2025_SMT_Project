final_glm_model <- glm(def_safe ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side, data = all_went_data_2, family = binomial)

all_final_data_5$glm_safe <- predict(final_glm_model, newdata = all_final_data_5, type = "response")

summary(final_glm_model)

Call:
glm(formula = def_safe ~ run_dist + OF_dist + run_speed + top_speed + 
    OF_momentum_home + OF_momentum_side, family = binomial, data = all_went_data_2)

Coefficients:
                  Estimate Std. Error z value Pr(>|z|)    
(Intercept)      -0.007934   3.663865  -0.002   0.9983    
run_dist         -0.126707   0.017885  -7.085 1.39e-12 ***
OF_dist           0.038564   0.008053   4.789 1.68e-06 ***
run_speed         0.127474   0.080853   1.577   0.1149    
top_speed         0.053205   0.131786   0.404   0.6864    
OF_momentum_home -0.067346   0.029042  -2.319   0.0204 *  
OF_momentum_side  0.053349   0.035253   1.513   0.1302    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 318.05  on 559  degrees of freedom
Residual deviance: 198.92  on 553  degrees of freedom
AIC: 212.92

Number of Fisher Scoring iterations: 8
all_final_data_5 <- all_final_data_5[,c(-24)] %>%
    relocate(glm_safe, .after = prob_to_go)

all_final_data_5 <- all_final_data_5 %>%
    mutate(went_home = ifelse(!is.na(safe_out), 1, 0)) %>%
    mutate(should_go = ifelse(glm_safe >= prob_to_go, 1, 0)) %>%
    mutate(correct_decision = ifelse(went_home == should_go, 1, 0))

mean(all_final_data_5$correct_decision)
[1] 0.913615

confusion_matrix <- table(Predicted = all_final_data_5$should_go, Actual = all_final_data_5$went_home)
print(confusion_matrix)
        Actual
Predicted   0   1
        0 449  36
        1  56 524

Didn’t send 9.7% of players who the model would send
Sent 7.4% of players the model wouldn’t send
glm_go_model <- glm(went_home ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side + prob_to_go, data = all_final_data_5, family = binomial)

all_final_data_5$glm_go <- predict(glm_go_model, type = "response")

summary(glm_go_model)

Call:
glm(formula = went_home ~ run_dist + OF_dist + run_speed + top_speed + 
    OF_momentum_home + OF_momentum_side + prob_to_go, family = binomial, 
    data = all_final_data_5)

Coefficients:
                   Estimate Std. Error z value Pr(>|z|)    
(Intercept)      -13.854990   2.983893  -4.643 3.43e-06 ***
run_dist          -0.181263   0.015445 -11.736  < 2e-16 ***
OF_dist            0.060903   0.006902   8.824  < 2e-16 ***
run_speed          0.124659   0.059246   2.104   0.0354 *  
top_speed          0.506380   0.087866   5.763 8.26e-09 ***
OF_momentum_home  -0.054247   0.024017  -2.259   0.0239 *  
OF_momentum_side   0.051663   0.027690   1.866   0.0621 .  
prob_to_go         1.444180   1.546060   0.934   0.3503    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 1473.56  on 1064  degrees of freedom
Residual deviance:  314.87  on 1057  degrees of freedom
AIC: 330.87

Number of Fisher Scoring iterations: 8
all_final_data_5 <- all_final_data_5 %>%
    mutate(safe_min_go = glm_safe - glm_go)
So + equals too conservative, - equals too aggressive

safe_min_go_glm <- glm(safe_min_go ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side + prob_to_go, data = all_final_data_5)

summary(safe_min_go_glm)
Call:
glm(formula = safe_min_go ~ run_dist + OF_dist + run_speed + 
    top_speed + OF_momentum_home + OF_momentum_side + prob_to_go, 
    data = all_final_data_5)

Coefficients:
                   Estimate Std. Error t value Pr(>|t|)    
(Intercept)       5.498e-01  4.538e-02  12.118  < 2e-16 ***
run_dist          4.294e-04  7.651e-05   5.613 2.54e-08 ***
OF_dist          -3.684e-04  9.311e-05  -3.957 8.10e-05 ***
run_speed         8.292e-03  1.083e-03   7.654 4.37e-14 ***
top_speed        -2.343e-02  1.570e-03 -14.923  < 2e-16 ***
OF_momentum_home -1.787e-03  4.228e-04  -4.227 2.57e-05 ***
OF_momentum_side  1.246e-05  5.477e-04   0.023    0.982    
prob_to_go       -1.276e-02  2.868e-02  -0.445    0.656    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for gaussian family taken to be 0.008743824)

    Null deviance: 11.7175  on 1064  degrees of freedom
Residual deviance:  9.2422  on 1057  degrees of freedom
AIC: -2015.2

Number of Fisher Scoring iterations: 2

Think this means… 
Too conservative with high run_dist / too aggressive with low run_dist
Too conservative with low OF_dist / too aggressive with high OF_dist
Too conservative with high run_speed / too aggressive with low run_speed
Too conservative with low top_speed / too aggressive with high top_speed
Too conservative with low OF_momentum_home / too aggressive with high OF_momentum_home
team_info <- full_join(game_info_2[,c(1, 16, 24)], game_events_2[,c(2, 12)], by = "ppg_key")

team_info <- team_info %>%
    mutate(play_key = paste(game_str, play_id, sep = "_")) %>%
    mutate(team = substr(batter, start = 1, stop = 3))

team_info <- team_info %>%
    distinct(play_key, .keep_all = TRUE)

all_final_data_6 <- left_join(all_final_data_5, team_info[,5:6], by = "play_key")

ACN AKX ALA APZ AVV BEJ BVD CGA CMS CRQ DMS DYE ESW FBP GAX GEA GHD GIS HCI HKR HMN IAK IAQ IHI 
  3   4   2   9   3   2   4   5   2   7   5  34   7  12   6   4   7   9   3  10   7   1   9   3 
IKJ IXC JFU JIL JJS JMJ JNJ JZK KIR KJH KNB KQQ LFS MCJ MGJ MHY MRJ NSO NYA OWH OXG PHS POW PTB 
 11   3   7   7   6   8   1   5   4   5  20   6   7   2   4  13  11   2   4  11  29   1   3   5 
PVJ QEA QZE RDO RQJ RZQ STK TEQ TKJ TSZ UEX UKI UPU UWE VHS VMN VZE WJU WMI WNA WZR XAX XFE XLB 
  6  71  11   5   5 217  12   2   5  11   7   6  10   2   5   3   8   9   6   6   5   2   9   2 
YJD YUH 
192   3

QEA_final_data <- all_final_data_6 %>%
    filter(team == "QEA")
QEA_confusion_matrix <- table(Predicted = QEA_final_data$should_go, Actual = QEA_final_data$went_home)
print(QEA_confusion_matrix)
        Actual
Predicted  0  1
        0 20  5
        1  6 40

Didn’t send 13.0% of players who the model would send
Sent 20% of players the model wouldn’t send


RZQ_final_data <- all_final_data_6 %>%
    filter(team == "RZQ")
RZQ_confusion_matrix <- table(Predicted = RZQ_final_data$should_go, Actual = RZQ_final_data$went_home)
print(RZQ_confusion_matrix)
        Actual
Predicted   0   1
        0  95   6
        1   9 107

Didn’t send 7.8% of players who the model would send
Sent 5.9% of players the model wouldn’t send


YJD_final_data <- all_final_data_6 %>%
    filter(team == "YJD")
YJD_confusion_matrix <- table(Predicted = YJD_final_data$should_go, Actual = YJD_final_data$went_home)
print(YJD_confusion_matrix)
        Actual
Predicted  0  1
        0 77  6
        1 12 97

Didn’t send 11% of players who the model would send
Sent 7.2% of players the model wouldn’t send
incorrect_decision <- all_final_data_6 %>%
    filter(correct_decision == 0)
all_final_data_scaled <- all_final_data_6

all_final_data_scaled[,c(2:7,23)] <- scale(all_final_data_6[,c(2:7,23)])

scaled_should_go <- glm(should_go ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side + prob_to_go, data = all_final_data_scaled, family = binomial)

coef(scaled_should_go)
    (Intercept)         run_dist          OF_dist        run_speed        top_speed 
        91.77071       -607.70968        229.73608         63.50010         15.78930 
OF_momentum_home OF_momentum_side       prob_to_go 
       -93.98406         34.02201        -65.48629

sd(coef(scaled_should_go))
[1] 249.3603

coef(scaled_should_go) / sd(coef(scaled_should_go))
     (Intercept)         run_dist          OF_dist        run_speed        top_speed 
      0.36802447      -2.43707432       0.92130159       0.25465196       0.06331921 
OF_momentum_home OF_momentum_side       prob_to_go 
     -0.37690058       0.13643715      -0.26261709


scaled_went <- glm(went_home ~ run_dist + OF_dist + run_speed + top_speed + OF_momentum_home + OF_momentum_side + prob_to_go, data = all_final_data_scaled, family = binomial)

coef(scaled_went)
    (Intercept)         run_dist          OF_dist        run_speed        top_speed 
       0.5945561       -6.8326076        2.8662002        0.4321529        1.2093262 
OF_momentum_home OF_momentum_side       prob_to_go 
      -0.5698051        0.2780468        0.1450003

sd(coef(scaled_went))
[1] 2.850687

coef(scaled_went) / sd(coef(scaled_went))
     (Intercept)         run_dist          OF_dist        run_speed        top_speed 
      0.20856591      -2.39682843       1.00544192       0.15159605       0.42422273 
OF_momentum_home OF_momentum_side       prob_to_go 
     -0.19988342       0.09753675       0.05086505

Third base coaches according to model…
Slightly don’t use OF_dist enough
Slightly don’t use run_speed enough
Use top_speed too much
Don’t use OF_momentum_home enough
Slightly don’t use OF_momentum_side enough
Don’t use prob_to_go / situation nearly enough
