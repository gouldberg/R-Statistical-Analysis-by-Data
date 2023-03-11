setwd("//media//kswada//MyFiles//R//ssocs")

packages <- c("dplyr", "MatchIt", "Matching", "survey")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

library(twang)


# combine the sampling weight with the stratum weight
# For estimating the ATE, the stratum weight is the stratum size divided by the total sample size
# For estimating the ATT, the stratum weight is the treated sample size within the stratum divided by the total treated sample size

data.stratification$stratumWeight <- with(data.stratification,  FINALWGT * weights / mean(FINALWGT * weights))


balanceResults <- list()


# loop through strata to perform balance evaluation
for (stratum in 1:5) {
  
  tempData <- data.stratification[data.stratification$subclass==stratum,]
  
  # balance evaluation for continuous covariates
  tableBalance <- bal.stat(
    data = tempData, estimand = "ATT",  
    w.all = tempData$stratumWeight, get.ks = F, vars = covariateNames, 
    treat.var = "treat", sampw = 1, multinom = F)
  tableBalance <- tableBalance$results[,1:5]
  
  # store results
  balanceResults[[stratum]] <- tableBalance
}



# ----------
# summarize covariate balance
balance.stratification2 <- lapply(balanceResults, summary)

balance.stratification2



# ------------------------------------------------------------------------------
# Estimating and Pooling Stratum-specific effects
# ------------------------------------------------------------------------------

library(survey)


surveyDesign <- svydesign(ids = ~ 0, strata = ~ STRATA, weights = ~ FINALWGT, data = data.stratification)


# add replication weights for 1000 bootstrapped samples to the design object
surveyDesignBoot <- as.svrepdesign(surveyDesign, type = c("bootstrap"), replicates = 1000)



# ----------
# Mean and standard error of proportion Harsh Punishment by combinations of treatment group and stratum
( subclassMeans <- svyby(formula = ~ percHarsh, by = ~ treat + subclass, design = surveyDesignBoot, FUN = svymean, covmat = TRUE) )



# ----------
# obtain the ATE pooling strata-specific ATEs

# the weights are for each group defined by combining stratum and treatment group
# the weights are (stratum size)/(total sample size)

( pooledEffects <- svycontrast(subclassMeans, list(
  ATE = c(-0.5,0.5,-0.18,0.18,-0.13,0.13,-0.10,0.10,-0.09,0.09),
  ATT = c(-0.2,0.2,-0.2,0.2,-0.2,0.2,-0.2,0.2,-0.2,0.2))) )



# -->
# Neither the ATE nor the ATT are statistically significant


# ----------
# regression adjustment
# regressionAdjuster = function(formula, design) {
#     model = svyglm(formula, design)
# (subclassMeansAdjusted <- svyby(percHarsh ~ FR_CATMN, by = ~ treat + subclass,
#                         design = surveyDesignBoot, FUN = svyglm, covmat = TRUE, deff = F))




