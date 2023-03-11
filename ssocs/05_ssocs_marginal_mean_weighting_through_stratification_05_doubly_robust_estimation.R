setwd("//media//kswada//MyFiles//R//ssocs")

packages <- c("dplyr", "MatchIt", "Matching", "survey")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Doubly Robust Estimation with MMWS
#   - When the treatment effect is estimated with the difference between weighted means of the treated and untreated groups, 
#     double robustness can be achieved by using regression estimation to obtain the means of each group.
#   - Estimates of the treatment effect obtained with weighted regression using the MMWS can be made doubly robust by adding covariates and/or dummy
#     stratum indicators in the model.
#   - The advantage of adding dummy stratum indicators rather than propensity scores is that it allows for nonlinear relationships between
#     propensity scores and the outcome, while adding the propensity score directly would require the inclusion of polynomial terms.
#   - Regardless of whether covariates and/or dummy stratum indicators are included in the model, it is critical to also include interactions between
#     these variables and the treatment indicator.
#   - It is also necessary to center continuous covariates around the grand mean (for the ATE) or the treated group mean (for the ATT) to ensure
#     that the estimate treatment effect can still be interpreted as the ATE or the ATT.
# ------------------------------------------------------------------------------

# The following code estimates the ATT with a weighted regression model where dummy stratum indicators as well as the seven covariates that did not meet
# the strict criterion for balance are included as predictors, with all two-way interactions.


# center continuous predictors on the mean of the treated
surveyDesignATT <- update(surveyDesignATT, C0540 = C0540 - mean(C0540[treat=="Treated"]),
                          C0568 = C0568 - mean(C0568[treat=="Treated"]),
                          C0544 = C0544 - mean(C0544[treat=="Treated"]),
                          C0558 = C0558 - mean(C0558[treat=="Treated"]))

modelATTDR <- svyglm(percHarsh ~ (treat + subclass + FR_SIZE + C0540 + C0158 + C0166 + C0568 + C0540 + C0544 + C0558)^2,  surveyDesignATT, family = gaussian())

summary(modelATTDR)

#save(list=ls(), file="Chapter4_All_results.Rdata", compress=T)


# -->
# The double robust ATT estimate is -0.026 (SE = 0.061, p = 0.672)



