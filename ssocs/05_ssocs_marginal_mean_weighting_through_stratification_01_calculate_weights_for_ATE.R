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
# Calculate MMWS for ATE
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Create table with the number of treated cases, untreated cases per stratume
# ------------------------------------------------------------------------------

# obtain the number of people per subclass by treatment combination
subclassbyTreat <- data.frame(xtabs(~ treat + subclass, data.stratification))

names(subclassbyTreat) <- c("treat", "subclass", "N.zs")

print(subclassbyTreat)



# ----------
# obtain the number of people per subclass
table.subclass <- data.frame(xtabs( ~ subclass, data.stratification))
 
names(table.subclass) <- c("subclass","N.s")

print(table.subclass)


# merge tables
( subclassbyTreat <- merge(subclassbyTreat, table.subclass) )



# ------------------------------------------------------------------------------
# Calculate MMWTs for ATE
# ------------------------------------------------------------------------------

# obtain estimates of proportion treated and untreated
( prop.treat <- svymean(~ treat, surveyDesign) )


# insert proportion treated into table
subclassbyTreat$pr.Z <- ifelse(subclassbyTreat$treat == "Treated", prop.treat[2], prop.treat[1])


# obtain the mmwts for ATE
subclassbyTreat$mmwsATE <- with(subclassbyTreat, N.s * pr.Z / N.zs)

print(subclassbyTreat)



# ------------------------------------------------------------------------------
# Merge
# ------------------------------------------------------------------------------

# merge weights with data.stratification
data.stratification <- merge(data.stratification, subclassbyTreat[,c(1,2,6)])

xtabs(~ mmwsATE + subclass, data.stratification)



# ------------------------------------------------------------------------------
# calcualte final weight and normalize
# ------------------------------------------------------------------------------

data.stratification$mmwsATEFinal <- data.stratification$mmwsATE * data.stratification$FINALWGT 

data.stratification$mmwsATEFinal <- data.stratification$mmwsATEFinal / mean(data.stratification$mmwsATEFinal)

