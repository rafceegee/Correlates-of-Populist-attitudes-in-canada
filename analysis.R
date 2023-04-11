##LOAD PACKAGES##
library(lavaan)
library(survey)
library(srvyr)
library(psych)
library(tidyverse)
library(dplyr)
library(correlation)
library(corrplot)
library(scales)
library(performance)
library(yhat)
library(leaps)
##LOAD DATASET##
load("2021_Canadian_Election_Study_v1.0.RData")
data <- table
data$pes21_weight_general_all[is.na(data$pes21_weight_general_all)] <- 0 #assign 0  to un-weighted cases 
#create weighted data set for correlations and descriptives 
dat_wt <- as_survey(data, weights = pes21_weight_general_all) 
data_wt <- as.data.frame(dat_wt)

#POPULISM MEASURE#
#recode predictors
data$pes21_populism_2R <- case_match(data$pes21_populism_2, 1 ~ 0, 2 ~ 0.25, 3 ~ 0.5, 4 ~ 0.75, 5 ~ 1)
data$pes21_populism_3R <- case_match(data$pes21_populism_3, 1 ~ 0, 2 ~ 0.25, 3 ~ 0.5, 4 ~ 0.75, 5 ~ 1)
data$pes21_populism_4R <- case_match(data$pes21_populism_4, 5 ~ 0, 4 ~ 0.25, 3 ~ 0.5, 2 ~ 0.75, 1 ~ 1)
data$pes21_populism_7R <- case_match(data$pes21_populism_7, 1 ~ 0, 2 ~ 0.25, 3 ~ 0.5, 4 ~ 0.75, 5 ~ 1)
data$pes21_populism_8R <- case_match(data$pes21_populism_8, 1 ~ 0, 2 ~ 0.25, 3 ~ 0.5, 4 ~ 0.75, 5 ~ 1)
#recode predictors in weighted data set
data_wt$pes21_populism_2R <- case_match(data_wt$pes21_populism_2, 1 ~ 0, 2 ~ 0.25, 3 ~ 0.5, 4 ~ 0.75, 5 ~ 1)
data_wt$pes21_populism_3R <- case_match(data_wt$pes21_populism_3, 1 ~ 0, 2 ~ 0.25, 3 ~ 0.5, 4 ~ 0.75, 5 ~ 1)
data_wt$pes21_populism_4R <- case_match(data_wt$pes21_populism_4, 5 ~ 0, 4 ~ 0.25, 3 ~ 0.5, 2 ~ 0.75, 1 ~ 1)
data_wt$pes21_populism_7R <- case_match(data_wt$pes21_populism_7, 1 ~ 0, 2 ~ 0.25, 3 ~ 0.5, 4 ~ 0.75, 5 ~ 1)
data_wt$pes21_populism_8R <- case_match(data_wt$pes21_populism_8, 1 ~ 0, 2 ~ 0.25, 3 ~ 0.5, 4 ~ 0.75, 5 ~ 1)

#cfa for populism measure with 5 predictors

pop <- 'popu =~  pes21_populism_2R + pes21_populism_3R + pes21_populism_4R  + pes21_populism_7R + pes21_populism_8R'
fit <- cfa(pop, sampling.weights = "pes21_weight_general_all", missing = "ML", data = data)
fita <- cfa(pop, missing = "ML", data = data_wt)
summary(fit, fit.measures = TRUE, standardized = TRUE)


#create factor in data set
data <- cbind(data, lavPredict(fit, type = "lv", append.data = TRUE))
data_wt <- cbind(data_wt, lavPredict(fita, type = "lv", append.data = TRUE)) #weighted factor
data$popu <- rescale(data$popu)
data_wt$popu <- rescale(data_wt$popu) #weighted factor
mean(data$popu, na.rm = TRUE) #mean of populism measure

##PSYCHOLOGICAL CORRELATES##
#cognition
data$cog <- ifelse(data$pes21_cognition >= 4, 1, 0)
#social trust
data$soc_trust <- ifelse(data$pes21_trust == 1, 1, 0)

##POLITICAL CORRELATES##
#feelings towards politicians
data$feelpol <- rescale(data$pes21_groups1_4)
data_wt$feelpol <- rescale(data_wt$pes21_groups1_4)
#discuss politics
data$dispol <- ifelse(data$pes21_discfam >= 3 & data$pes21_discfam < 5, 1, 0)
#government ineffectiveness
data$govinef <- ifelse(data$pes21_govtprograms >= 4 & data$pes21_govtprograms <6, 1, 0 )
#efficacy
data$govef <- ifelse(data$pes21_govtcare >= 4 & data$pes21_govtcare <6, 1, 0 )
#economic attitudes
data$pes21_trade <- as.vector(data$pes21_trade)
data$pes21_privjobs <- as.vector(data$pes21_privjobs)
data$pes21_blame <- as.vector(data$pes21_blame)
data_wt$pes21_trade <- as.vector(data_wt$pes21_trade)
data_wt$pes21_privjobs <- as.vector(data_wt$pes21_privjobs)
data_wt$pes21_blame <- as.vector(data_wt$pes21_blame)
eco <- 'econ =~ pes21_trade + pes21_privjobs + pes21_blame'
fit3 <- cfa(eco, sampling.weights = "pes21_weight_general_all", missing = "ML", data = data)
fit3a <- cfa(eco, missing = "ML", data = data)
summary(fit3, fit.measures = TRUE, standardized = TRUE)
data <- cbind(data, lavPredict(fit3, type = "lv", append.data = TRUE))
data_wt <- cbind(data_wt, lavPredict(fit3, type = "lv", append.data = TRUE)) #weighted
data$econ <- rescale(data$econ)
data_wt$econ <- rescale(data_wt$econ) #weighted
#satisfaction with democracy
data$demsat <- ifelse(data$pes21_dem_sat <= 2, 1, 0)
##CANADA SPECIFIC VARIABLES## 
#western Canada
data$west <- ifelse(data$Region == "West", 1, 0)
#indigenous resentment 
data$pes21_ab_favors <- as.vector(data$pes21_ab_favors)
data$pes21_ab_deserve <- as.vector(data$pes21_ab_deserve)
data$pes21_ab_col <- as.vector(data$pes21_ab_col)
data_wt$pes21_ab_favors <- as.vector(data_wt$pes21_ab_favors)
data_wt$pes21_ab_deserve <- as.vector(data_wt$pes21_ab_deserve)
data_wt$pes21_ab_col <- as.vector(data_wt$pes21_ab_col)
ind <- 'indi =~ pes21_ab_favors + pes21_ab_deserve + pes21_ab_col'
fit4 <- cfa(ind, sampling.weights = "pes21_weight_general_all", missing = "ML", data = data)
fit4a <- cfa(ind, missing = "ML", data = data_wt)
summary(fit4, fit.measures = TRUE, standardized = TRUE)
data <- cbind(data, lavPredict(fit4, type = "lv", append.data = TRUE))
data_wt <- cbind(data_wt, lavPredict(fit4, type = "lv", append.data = TRUE)) #weighted
data$indi <- rescale(data$indi)
data_wt$indi <- rescale(data_wt$indi) #weighted
#Quebec
data$pes21_langQC <- as.vector(data$pes21_langQC)
data$pes21_cultureQC <- as.vector(data$pes21_cultureQC)
data$pes21_qclang <- as.vector(data$pes21_qclang)
data$pes21_qcsol <- as.vector(data$pes21_qcsol)
data_wt$pes21_langQC <- as.vector(data_wt$pes21_langQC)
data_wt$pes21_cultureQC <- as.vector(data_wt$pes21_cultureQC)
data_wt$pes21_qclang <- as.vector(data_wt$pes21_qclang)
data_wt$pes21_qcsol <- as.vector(data_wt$pes21_qcsol)
qeb <- "qebc =~ pes21_langQC + pes21_cultureQC + pes21_qclang + pes21_qcsol"
fit5 <- cfa(qeb, sampling.weights = "pes21_weight_general_all", missing = "ML", data = data)
fit5a <- cfa(qeb, missing = "ML", data = data_wt)
summary(fit5, fit.measures = TRUE, standardized = TRUE)
data <- cbind(data, lavPredict(fit5, type = "lv", append.data = TRUE))
data_wt <- cbind(data_wt, lavPredict(fit5, type = "lv", append.data = TRUE)) #weighted data set for correlations
data$qebc <- rescale(data$qebc)
data_wt$qebc <- rescale(data_wt$qebc)#weighted data set for correlations
#being Canadian
data$canbe <- ifelse(data$pes21_ethid_1 >= 3 & data$pes21_ethid_1 <5, 1, 0)
#feeling towards Americans
data$cps21_groups_therm_7 <- rescale(data$cps21_groups_therm_7)
data_wt$cps21_groups_therm_7 <- rescale(data_wt$cps21_groups_therm_7)
#feelings towards parties
data$lib <- rescale(data$cps21_party_rating_23)
data_wt$lib <- rescale(data_wt$cps21_party_rating_23) #weighted 
data$con <- rescale(data$cps21_party_rating_24)
data_wt$con <- rescale(data_wt$cps21_party_rating_24) #weighted
data$ndp <- rescale(data$cps21_party_rating_25)
data_wt$ndp <- rescale(data_wt$cps21_party_rating_25) #weighted
data$pq <- rescale(data$cps21_party_rating_26)
data_wt$pq <- rescale(data_wt$cps21_party_rating_26) #weighted
data$gpc <- rescale(data$cps21_party_rating_27)
data_wt$gpc <- rescale(data_wt$cps21_party_rating_27) #weighted
data$ppc <- rescale(data$cps21_party_rating_29)
data_wt$ppc <- rescale(data_wt$cps21_party_rating_29) #weighted
##CONTROL VARIABLES##
#gender
data$wom <- ifelse(data$cps21_genderid == 2, 1, 0)
#post secondary education
data$uni <- ifelse(data$cps21_education >= 6 & data$cps21_education < 12, 1, 0)
#unemployed
data$unem <- ifelse(data$cps21_employment == 5, 1, 0)
#trust 
data$pes21_conf_inst1_1R <- case_match(data$pes21_conf_inst1_1, 4 ~ 0, 3 ~ 0.333, 2 ~ 0.667, 1 ~  1)
data$pes21_conf_inst1_2R <- case_match(data$pes21_conf_inst1_2, 4 ~ 0, 3 ~ 0.333, 2 ~ 0.667, 1 ~  1)
data$pes21_conf_inst1_4R <- case_match(data$pes21_conf_inst1_4, 4 ~ 0, 3 ~ 0.333, 2 ~ 0.667, 1 ~  1)
data$pes21_conf_inst2_1R <- case_match(data$pes21_conf_inst2_1, 4 ~ 0, 3 ~ 0.333, 2 ~ 0.667, 1 ~  1)
data$pes21_conf_inst2_5R <- case_match(data$pes21_conf_inst2_5, 4 ~ 0, 3 ~ 0.333, 2 ~ 0.667, 1 ~  1)
#weighted data set
data_wt$pes21_conf_inst1_1R <- case_match(data_wt$pes21_conf_inst1_1, 4 ~ 0, 3 ~ 0.333, 2 ~ 0.667, 1 ~  1)
data_wt$pes21_conf_inst1_2R <- case_match(data_wt$pes21_conf_inst1_2, 4 ~ 0, 3 ~ 0.333, 2 ~ 0.667, 1 ~  1)
data_wt$pes21_conf_inst1_4R <- case_match(data_wt$pes21_conf_inst1_4, 4 ~ 0, 3 ~ 0.333, 2 ~ 0.667, 1 ~  1)
data_wt$pes21_conf_inst2_1R <- case_match(data_wt$pes21_conf_inst2_1, 4 ~ 0, 3 ~ 0.333, 2 ~ 0.667, 1 ~  1)
data_wt$pes21_conf_inst2_5R <- case_match(data_wt$pes21_conf_inst2_5, 4 ~ 0, 3 ~ 0.333, 2 ~ 0.667, 1 ~  1)
tru <- 'trus =~ pes21_conf_inst1_1R + pes21_conf_inst1_2R + pes21_conf_inst1_4R + pes21_conf_inst2_1R + pes21_conf_inst2_5R'
fit2 <- cfa(tru, sampling.weights = "pes21_weight_general_all", missing = "ML", data = data)
fit2a <- cfa(tru, missing = "ML", data = data_wt)
summary(fit2, fit.measures = TRUE, standardized = TRUE)
#create factors
data <- cbind(data, lavPredict(fit2, type = "lv", append.data = TRUE))
data_wt <- cbind(data_wt, lavPredict(fit2a, type = "lv", append.data = TRUE)) #weighted data set
data$trus <- rescale(data$trus)
data_wt$trus <- rescale(data_wt$trus) #weighted data set

#PYSCHOLOGICAL CORRELATES#
##correlation matrix##
#subset data
pysc <- subset(data_wt, select = c("popu", "pes21_big5_1",
                               "pes21_big5_2", "pes21_big5_3", "pes21_big5_4", "pes21_big5_5",
                                "pes21_big5_6", "pes21_big5_7", "pes21_big5_8", "pes21_big5_9", "pes21_big5_10", "pes21_feminine_1",
                               "pes21_masculine_1", "trus"
                               ))

correlation(pysc)

#remove non-significant personality traits
pysc2 <- subset(data, select = c("popu", "pes21_big5_3", "pes21_big5_9", "pes21_feminine_1", "pes21_masculine_1"))
correlation(pysc2)
corrplot(cor(pysc2, use = "pairwise.complete.obs"), method = "number", type = "upper")

##REGRESSION MODEL FOR PYSCHOLOGICAL CORRELATES##
Mod1 <- lm(popu ~ pes21_big5_3 + pes21_big5_9 + pes21_feminine_1 + pes21_masculine_1 + cog + soc_trust + trus + wom + uni + unem + cps21_age + cps21_lr_scale_bef_1, data = data, na.action = na.omit, weights = pes21_weight_general_all)
summary(Mod1)
performance(Mod1)

#POLITICAL CORRELATES#
#correlation matrix
poli <- subset(data_wt, select = c("feelpol", "econ", "popu"))
correlation(poli)
corrplot(cor(poli, use = "pairwise.complete.obs"), method = "number", type = "upper")

###REGRESSION MODEL FOR POLITICAL CORRELATES##
Mod2 <- lm(popu ~ feelpol + dispol + govinef + govef + econ + demsat +trus + wom + uni + unem + cps21_age + cps21_lr_scale_bef_1, data = data, na.action = na.omit, weights = pes21_weight_general_all )
summary(Mod2)
performance(Mod2)

##CANADA SPECIFIC CORRELATES##
cansp <- subset(data_wt, select = c("popu", "indi", "cps21_groups_therm_7", 
                                 "lib", "con", "ndp", "gpc", "pq", "ppc" ))
correlation(cansp)
corrplot(cor(cansp, use = "pairwise.complete.obs"), method = "number", type = "upper")

##REGRESSION MODEL FOR CANADA SPECIFIC CORRELATES##
Mod3 <- lm(popu ~ west + indi + canbe + cps21_groups_therm_7 + lib + con + ndp + gpc + pq + ppc +
             trus + wom + uni + unem + cps21_age + cps21_lr_scale_bef_1, data = data, na.action = na.omit, weights = pes21_weight_general_all)
summary(Mod3)

Mod4 <- lm(popu ~ lib + con + ndp + pq + ppc + feelpol + dispol + govinef + govef + econ + demsat + cog + soc_trust + 
             trus + wom + uni + unem + cps21_age + cps21_lr_scale_bef_1, data = data, na.action = na.omit, weights = pes21_weight_general_all)
summary(Mod4)

Reg1 <- regr(Mod4)
RegOut_1$Beta_Weights
RegOut_1$Structure_Coefficients
RegOut_1$Commonality_Data
