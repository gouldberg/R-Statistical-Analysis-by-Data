# setwd("//media//kswada//MyFiles//R//lalonde_nswcps")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\algebra_nation")

packages <- c("dplyr", "tidyverse", "MatchIt", "Matching", "survey")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# Model the outcome as a function of the treatment and GPS
# Just intermediate step to obtaining individual treatment effects
# ------------------------------------------------------------------------------

library(survey)


# In this particular example, the lm or glm functions could have been used instead,
# because the data do not contain sampling weights.

designAN <- svydesign(id = ~1, weights = ~1, data = biased_data)



# outcome is the school-level mean Algebra EOC assessment score in 2014.
# For the current example, the model contains linear and quadratic effects of treatment and GPS,
# as well as the interaction between them.


modelOutcome0_svy <- svyglm(formula = "spend ~ treatment + GPS", design = designAN)


modelOutcome1_svy <- svyglm(formula = "spend ~ treatment + GPS + I(GPS ^ 2)", design = designAN)





# ----------
# lm model

modelOutcome0 <- lm(formula = "spend ~ treatment + GPS", data = biased_data)


modelOutcome1 <- lm(formula = "spend ~ treatment + GPS + I(GPS ^ 2)", data = biased_data)



stargazer::stargazer(modelOutcome0, modelOutcome1, type = "text")




# -->
# For both model, GPS is sgnificant ...




# ----------

car::residualPlots(modelOutcome0)

car::residualPlots(modelOutcome1)




# ------------------------------------------------------------------------------
# Estimate Dose-Response Function
# ------------------------------------------------------------------------------


# this is not appropriate analysis ...

quantile(biased_data$treatment, probs = seq(0.01, 1, 0.01))





# mod_obj <- modelOutcome0_svy

mod_obj <- modelOutcome1_svy




all.effects <- data.frame()


for (dose in quantile(biased_data$treatment, probs = seq(0.01, 1, 0.01)) ) {
  
  # predict outcome given all the GPS for a fixed value of dosage
  effects <- predict(mod_obj, type = "response", vcov = T,
                     newdata = data.frame(treatment = dose, GPS = biased_data$GPS))
  
  # 1/nrow(data):  average effect
  effect <- svycontrast(effects, rep(1 / nrow(biased_data), nrow(biased_data)))
  
  all.effects <- rbind(all.effects, data.frame(effect))
}




# ----------
doseResponses <- data.frame(percentile = seq(1, 100, 1),
                            treatment = quantile(biased_data$treatment, probs = seq(0.01, 1, 0.01)),
                            all.effects)


names(doseResponses)[3:4] <- c("spend", "SE")




# ----------
# calculate confidence intervals

doseResponses$lowerCL <- with(doseResponses, spend - 1.96 * SE)

doseResponses$upperCL <- with(doseResponses, spend + 1.96 * SE)





# ------------------------------------------------------------------------------
# plot dose-response function
# ------------------------------------------------------------------------------

with(doseResponses, plot(treatment, spend, main = "", type = "b"))


with(doseResponses, lines(treatment, lowerCL, lty = 'dashed'))


with(doseResponses, lines(treatment, upperCL, lty = 'dashed'))




