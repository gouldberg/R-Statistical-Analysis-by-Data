# setwd("//media//kswada//MyFiles//R//lalonde_nswcps")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\algebra_nation")

packages <- c("dplyr", "tidyverse", "MatchIt", "Matching", "survey")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data: Algebra Nation
# ------------------------------------------------------------------------------


data <- read.csv("algebra_nation.csv")


str(data)


car::some(data)


names(data)




# ----------
# Standardize continuous covariates

data$numOfStud2014 <- scale(data$numOfStud2014)

data$meanScale2012 <- scale(data$meanScale2012)

data$lev1Perc2012 <- scale(data$lev1Perc2012)

data$lev5Perc2012 <- scale(data$lev5Perc2012)

data$perc.free.lunch <- scale(data$perc.free.lunch)

data$perc.reduced.lunch <- scale(data$perc.reduced.lunch)



# ----------
# convert binary variables to factors

data$SeniorHigh <- factor(data$SeniorHigh)

data$middleHigh <- factor(data$middleHigh)




# ----------
# breakdown locale into size and type variables

data$locationSize <- with(data, ifelse(Locale.=="City: Large" |
                                         Locale.=="Suburb: Large", "Large", ifelse( Locale.=="City: Midsize"|
                                                                                      Locale.=="Suburb: Midsize", "Midsize", "Small")))  
data$locationSize <- factor(data$locationSize)



data$locationRural <- with(data, ifelse(Locale.=="Rural: Distant" |
                                          Locale.=="Rural: Fringe" |
                                          Locale.=="Rural: Remote", "Rural", "Urban"))  

data$locationRural <- factor(data$locationRural)




# ------------------------------------------------------------------------------
# Model the outcome as a function of the treatment and GPS
# Just intermediate step to obtaining individual treatment effects
# ------------------------------------------------------------------------------

library(survey)


# In this particular example, the lm or glm functions could have been used instead,
# because the data do not contain sampling weights.

designAN <- svydesign(id = ~1, weights = ~1, data = data)



# outcome is the school-level mean Algebra EOC assessment score in 2014.
# For the current example, the model contains linear and quadratic effects of treatment and GPS,
# as well as the interaction between them.


modelOutcome0_svy <- svyglm(formula = "meanScale2014 ~ logLoginsPerExaminee + GPS", design = designAN)


modelOutcome1_svy <- svyglm(formula = "meanScale2014 ~ logLoginsPerExaminee + GPS + I(GPS ^ 2)", design = designAN)


modelOutcome2_svy <- svyglm(formula = "meanScale2014 ~ logLoginsPerExaminee + GPS + I(GPS ^ 2) + I(logLoginsPerExaminee ^ 2)", design = designAN)


modelOutcome3_svy <- svyglm(formula = "meanScale2014 ~ logLoginsPerExaminee + GPS + logLoginsPerExaminee : GPS", design = designAN)


modelOutcome4_svy <- svyglm(formula = "meanScale2014 ~ logLoginsPerExaminee + I(logLoginsPerExaminee ^ 2) + 
                                          GPS + I(GPS ^ 2) + logLoginsPerExaminee : GPS", design = designAN)




# ----------
# lm model

modelOutcome0 <- lm(formula = "meanScale2014 ~ logLoginsPerExaminee + GPS", data = data)


modelOutcome1 <- lm(formula = "meanScale2014 ~ logLoginsPerExaminee + GPS + I(GPS ^ 2)", data = data)


modelOutcome2 <- lm(formula = "meanScale2014 ~ logLoginsPerExaminee + GPS + I(GPS ^ 2) + I(logLoginsPerExaminee ^ 2)", data = data)


modelOutcome3 <- lm(formula = "meanScale2014 ~ logLoginsPerExaminee + GPS + logLoginsPerExaminee : GPS", data = data)


modelOutcome4 <- lm(formula = "meanScale2014 ~ logLoginsPerExaminee + I(logLoginsPerExaminee ^ 2) + 
                                          GPS + I(GPS ^ 2) + logLoginsPerExaminee : GPS", data = data)



stargazer::stargazer(modelOutcome0, modelOutcome1, modelOutcome2, modelOutcome3, modelOutcome4, type = "text")




# -->
# modelOutcome0:  coefficient of "logLoginsPerExaminee" = 0.466
# modelOutcome4:  coefficient of "logLoginsPerExaminee" = 0.593
# modelOutcome0 and 3:  the GPS is significant ... not good  --> I(GPS ^ 2)  is required ...


# NOTE that no GPS matching is required here.




# ----------

car::residualPlots(modelOutcome0)

car::residualPlots(modelOutcome1)

car::residualPlots(modelOutcome2)

car::residualPlots(modelOutcome3)

car::residualPlots(modelOutcome4)


# ------------------------------------------------------------------------------
# Estimate Dose-Response Function
# ------------------------------------------------------------------------------


# mod_obj <- modelOutcome0_svy

mod_obj <- modelOutcome4_svy




all.effects <- data.frame()


for (dose in quantile(data$logLoginsPerExaminee, probs = seq(0.01, 1, 0.01)) ) {
  
  # predict outcome given all the GPS for a fixed value of dosage
  effects <- predict(mod_obj, type = "response", vcov = T,
                     newdata = data.frame(logLoginsPerExaminee = dose, GPS = data$GPS))
  
  # 1/nrow(data):  average effect
  effect <- svycontrast(effects, rep(1 / nrow(data), nrow(data)))
  
  all.effects <- rbind(all.effects, data.frame(effect))
}




# ----------
doseResponses <- data.frame(percentile = seq(1, 100, 1),
                            logLoginsPerExaminee = quantile(data$logLoginsPerExaminee, probs = seq(0.01, 1, 0.01)),
                            all.effects)


names(doseResponses)[3:4] <- c("meanScale2014", "SE")




# ----------
# calculate confidence intervals

doseResponses$lowerCL <- with(doseResponses, meanScale2014 - 1.96 * SE)

doseResponses$upperCL <- with(doseResponses, meanScale2014 + 1.96 * SE)





# ------------------------------------------------------------------------------
# plot dose-response function
# ------------------------------------------------------------------------------

with(doseResponses, plot(logLoginsPerExaminee,meanScale2014, main = "", type = "b", ylim = c(380, 420),
                         xlab = "Log logins per examinee", ylab = "Mean Algebra I 2014") )


with(doseResponses, lines(logLoginsPerExaminee, lowerCL, lty = 'dashed'))


with(doseResponses, lines(logLoginsPerExaminee, upperCL, lty = 'dashed'))



# -->
# The treatment does effects can be interpreted as the expected values of the outcome
# if all the participants received each specific does of treatment.

# confidence intervals are narrower in the middle of the distribution of log logins per examinee,
# because more schools were avaiable to estimate the treatment does effects at the middle region of the distribution



