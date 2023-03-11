setwd("//media//kswada//MyFiles//R//ssocs")

packages <- c("dplyr", "MatchIt", "Matching", "survey")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Marginal Mean Weighting Through Stratification
#   - consists of creating weights based on strata membership that adjust for the difference between the observed proportions of treated and untreated
#     units wtihin strata and the proportions that would  be obtained if randomized treatment assignment was used.
#   - This is similar to poststratification in survey data analysis because poststratification adjusts for differences between sample proportions
#     within strata and population proportions.
#   - One major advantage of obtaining a marginal treatment effect across strata over pooling strata-specific treatment effects is that
#     the former requires only marginal covariate balance, while the latter requires covariate balance within each stratum.
#     Within-stratum covariate balance is more difficult to obtain because small stratum sizes lead to unreliable within-stratum group means for
#     each covariate.
# 
#   - The fact that weights for MMWS are computed from strata rather than directly from propensity scores brings the advantage that MMWS is less likely
#     to produce extreme weights, which can affect the performance of propensity score weighting.
#   - MMWS is more robust to misspecifications of the propensity score model than inverse probability of treatment weighting because misspecification
#     to a certain degree may change the propensity scores but not affect strata membership.
#   - MMWS is expected to produce a smaller degree of bias reduction than propensity score weighting because of the coarseness of the weights,
#     but bias reduction is expected to increase as the number of strata increases.
#
#   - Because propensity score stratification is expected to provide less bias reduction than matching or weighting, it is always advisable to
#     combine propensity score stratification with either another propensity score method or doubly robust estimation.
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Calculate MMWS for ATT
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Create table with the number of treated cases, untreated cases per stratume
# ------------------------------------------------------------------------------

# obtain the number of people per subclass for the treated
subclassTreat <- data.frame(table(data.stratification$subclass[data.stratification$treat == "Treated"]))

names(subclassTreat) <- c("subclass","N.1s")

print(subclassTreat)



# ----------
# obtain the number of people per subclass for the untreated
subclassUntreat <-  data.frame(table(data.stratification$subclass[data.stratification$treat == "Untreated"]))

names(subclassUntreat) <- c("subclass","N.0s")

print(subclassUntreat)



# ----------
# merge treated and untreated
( table.subclass <- merge(subclassTreat, subclassUntreat) )

# merge table with the data
data.stratification <- merge(data.stratification, table.subclass)




# ------------------------------------------------------------------------------
# Calculate MMWTs for ATT
# ------------------------------------------------------------------------------

# obtain marginal proportions of treated and untreated cases
( prop.treat <- svymean(~ treat, surveyDesign) )



# ----------
# calculate the weight for the ATT
data.stratification$mmwsATT <- with(data.stratification, ifelse(treat == "Treated", 1, N.1s * prop.treat[1] / N.0s * prop.treat[2]))

xtabs(~ mmwsATT + subclass, data.stratification)



# ------------------------------------------------------------------------------
# calcualte final weight and normalize
# ------------------------------------------------------------------------------

data.stratification$mmwsATTFinal <- data.stratification$mmwsATT * data.stratification$FINALWGT 

data.stratification$mmwsATTFinal <- data.stratification$mmwsATTFinal / mean(data.stratification$mmwsATTFinal)



