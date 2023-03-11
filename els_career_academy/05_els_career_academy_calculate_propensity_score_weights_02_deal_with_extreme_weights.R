setwd("//media//kswada//MyFiles//R//els_career_academy")

packages <- c("dplyr", "survey")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# Deal with extreme weights
#   - Four strategies can be used to deal with extreme weights:
#    (1) respecify the propensity score model to check whether extreme weights are due to model misspecification
#    (2) change the propensity score method (e.g., marginal mean weighting through stratification)
#    (3) perform weight truncation
#    (4) use stabilized weights
# ------------------------------------------------------------------------------

# (3) weight truncation: 
# check the 99th percentile weight:
with(ELS.data.imputed, quantile(weightATE, 0.99))


# check how many observatios will have truncated weights
with(ELS.data.imputed, sum(weightATE > quantile(weightATE, 0.99)))


# truncate weights at the 99th percentile:
ELS.data.imputed$weightATETruncated <- with(ELS.data.imputed, ifelse(weightATE > quantile(weightATE, 0.99),  quantile(weightATE, 0.99), weightATE))    


# check truncated weights
with(ELS.data.imputed, by(weightATETruncated, treat, summary))




# ----------
# (4) stabilized weights
# obtain stabilized weights for the estimation of ATE
# which are obtained by multiplying the weights of the treated units by the constant c1, and the weights of the untreated units by the constant c0,
# which are equal to the expected values of being in the treated or untreated group, respectively.

ELS.data.imputed$C <- with(ELS.data.imputed, ifelse(treat == 1, pScores, 1 - pScores))


surveyDesign <- svydesign(ids = ~ psu, strata = ~ STRAT_ID, weights = ~ bystuwt, data = ELS.data.imputed, nest = T) #recreate survey design


( constants <- svyby(~ C, by = ~ treat, design = surveyDesign, FUN = svymean) )


ELS.data.imputed$stabilizedWeightATE <- ifelse(ELS.data.imputed$treat == 1, constants[1,2] / ELS.data.imputed$C, 
                                               constants[2,2] / ELS.data.imputed$C)


# check stabilized weights for extremeness
with(ELS.data.imputed, by(stabilizedWeightATE, treat, summary))



# -->
# One limitation of stabilized weights with cross-sectional studies is that the reduction of extreme weights may not be substantial.
# The maximum stabilized weights is 172.8, which is not very different from the maximum original weight of 187.8



# ----------
# compare the distributions of weights and stabilized weights for the ATE
graphics.off()

par(mfrow = c(1,1))
with(ELS.data.imputed, hist(weightATE[treat==1], density = 10, angle = 45, main="Propensity Score Weights for the ATE", xlab="Shaded = IPTW | Gray = Stabilized IPTW") )

with(ELS.data.imputed, hist(stabilizedWeightATE[treat==1], col=gray(0.4,0.25), add=T) )


