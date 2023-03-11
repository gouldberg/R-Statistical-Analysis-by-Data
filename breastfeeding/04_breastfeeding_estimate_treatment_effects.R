setwd("//media//kswada//MyFiles//R//breastfeeding")

packages <- c("dplyr", "MatchIt", "Matching", "survey")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# Estimate ATT with one-to-one greedy matching with replacement within 0.25 caliper
# ------------------------------------------------------------------------------

# obtain matched data
data.greedyMatching <- match.data(greedyMatching)



library(survey)
design.greedyMatching <- svydesign(ids = ~ 1, weights = ~ weights, data = data.greedyMatching)


# estimate the ATT 
model.greedyMatching <- svyglm(C0338600 ~ childCare, design.greedyMatching, family = gaussian())

summary(model.greedyMatching)




# ------------------------------------------------------------------------------
# #stimate ATT with variable ratio greedy matching with replacement and within 0.25 caliper 
# standard error is obtained with Abadie and Imbens (2006) estimator
# ------------------------------------------------------------------------------
summary(greedyMatching2)



# ------------------------------------------------------------------------------
# Estimate the ATT with variable ratio genetic matching with replacement within 0.25 caliper
# standard error is obtained with Abadie and Imbens (2006) estimator
# ------------------------------------------------------------------------------
summary(geneticMatching)



#-----------------------------------------
# Repeat the genetic matching, but this time with bias adjustment
# Abadie and Imbens showed that the matching estimator will be biased if the matching is not exact, but this bias can reduced by 
# regressing the outcomes on covariates only with the matched data.

geneticMatchingBA <- Match(Y = data$C0338600, Tr = data$childCare, X = covariateData,
                           BiasAdjust = T, Z = covariateData,
                           Weight.matrix = geneticWeights, estimand = "ATT", 
                           M = 1,  replace = TRUE, ties = TRUE)

summary(geneticMatchingBA)



# ------------------------------------------------------------------------------
# Estimate ATT with variable ratio  genetic matching without replacement within 0.25 caliper using regression
# ------------------------------------------------------------------------------

# obtain matched data
data.geneticMatching2 <- match.data(geneticMatching2)


# estimate the treatment effect
library(survey)
design.geneticMatching2 <- svydesign(ids = ~ 1, weights = ~ weights,
                                     data = data.geneticMatching2)


# fit regression model
model.geneticMatching2 <- svyglm(C0338600~childCare, design.geneticMatching2, family=gaussian())

summary(model.geneticMatching2)



# ------------------------------------------------------------------------------
# Estimate ATT with one-to-one optimal matched data using regression
# ------------------------------------------------------------------------------

# obtain matched data
data.optimalMatching <- match.data(optimalMatching)

library(survey)
design.optimalMatching <- svydesign(ids = ~ subclass, weights = ~ weights,
                                    data = data.optimalMatching)


# fit regression model
model.optimalMatching <- svyglm(C0338600 ~ childCare, design.optimalMatching, family=gaussian())

summary(model.optimalMatching)




# ------------------------------------------------------------------------------
# Estimate ATT with optimal full matched data using regression
# ------------------------------------------------------------------------------

# obtain matched data
data.fullMatching <- match.data(fullMatching)


# estimate the treatment effect
library(survey)
design.fullMatching <- svydesign(ids = ~ 1, weights = ~ weights,
                                 data = data.fullMatching)


# fit regression model
model.fullMatching <- svyglm(C0338600 ~ childCare, design.fullMatching, family = gaussian())

summary(model.fullMatching)



# ----------
# estimate treatment effects adjusting for cluster effects
design.fullMatching2 <- svydesign(ids = ~ subclass, weights = ~ weights, data = data.fullMatching)
design.fullMatching2 <- as.svrepdesign(design.fullMatching2, type="bootstrap", replicates=1000)


# fit regression model
model.fullMatching2 <- svyglm(C0338600 ~ childCare, design.fullMatching2, family = gaussian())
summary(model.fullMatching2)



# ----------
# estimate doubly robust ATT
outcomeModelDR <- svyglm(C0338600 ~ , design.fullMatching2)




