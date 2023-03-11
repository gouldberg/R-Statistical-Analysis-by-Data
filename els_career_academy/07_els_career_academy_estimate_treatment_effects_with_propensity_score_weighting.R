setwd("//media//kswada//MyFiles//R//els_career_academy")

packages <- c("dplyr", "survey")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# calculate a final weights and normalize
# ------------------------------------------------------------------------------

# create a final weights for estimating the effect of career academy participation on income in the second folow up (2006) 

ELS.data.imputed$finalWeight2006 <- with(ELS.data.imputed, bystuwt * weightATT)


# normalize the base-year to second folow-up (2006) final weight

ELS.data.imputed$finalWeight2006 <- ELS.data.imputed$finalWeight2006 / mean(ELS.data.imputed$finalWeight2006)


# check distribution of final weights

summary(ELS.data.imputed$finalWeight2006)




# ------------------------------------------------------------------------------
# Survey design with final weight for 2006
# ------------------------------------------------------------------------------

# re-create the survey design including the final weight for 2006

surveyDesign2006 <- svydesign(ids = ~ psu, strata = ~ STRAT_ID, weights = ~ finalWeight2006, data = ELS.data.imputed, nest = T)




# ------------------------------------------------------------------------------
# create replicate weights for bootstrapping
# ------------------------------------------------------------------------------

# IT TAKES TIME:  1 or 2 min.
surveyDesign2006Boot <- as.svrepdesign(surveyDesign2006, type = c("bootstrap"), replicates = 1000)





# ------------------------------------------------------------------------------
# Estimate ATT
# as weighted mean differences of earnings at the second follow-up
# ------------------------------------------------------------------------------

# weighted means for each group
( weightedMeans <- svyby(formula = ~ F2ERN5P2, by = ~ treat, design = surveyDesign2006Boot, FUN = svymean, covmat = TRUE) )


( ATT2006 <- svycontrast(weightedMeans, contrasts = c(-1, 1)) )



# ----------
# weighted variances for each group (to be used to obtain standardized effect size)
( weightedVars <- svyby(formula = ~ F2ERN5P2, by = ~ treat, design = surveyDesign2006Boot, FUN = svyvar, covmat = TRUE) )




# ------------------------------------------------------------------------------
# Estimate ATT
# by regression
# ------------------------------------------------------------------------------

# estimate the ATT for 2006 with regression analysis for complex survey data
outcomeModel2006 <- svyglm(F2ERN5P2 ~ treat, surveyDesign2006)

summary(outcomeModel2006)




# ------------------------------------------------------------------------------
# Estimate ATT
# by regression with standard errors estimated nonparametric bootstrapping
# ------------------------------------------------------------------------------

outcomeModel2006Boot <- svyglm(F2ERN5P2 ~ treat, surveyDesign2006Boot)

summary(outcomeModel2006Boot)

summary(outcomeModel2006)



# -->
# It should be noted that the standard errors obtained with Taylor series linearization and bootstrapping methods are very similar.


