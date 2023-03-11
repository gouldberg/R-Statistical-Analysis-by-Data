setwd("//media//kswada//MyFiles//R//ssocs")

packages <- c("dplyr", "MatchIt", "Matching", "survey")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Extract covariate balance table
# ------------------------------------------------------------------------------

# Extract a covariate balance table
balance.stratification <- summary(stratification, standardize=T)

head(balance.stratification)


# -->
# The covariate balance table is very large, because there were 67 covariates.



# ------------------------------------------------------------------------------
# Standard Mean Differenc for all strata
# ------------------------------------------------------------------------------

# Subclass 1
head(balance.stratification$q.table[,,1])


# 3:  Std.Mean Difference
strataDifferences <- data.frame(balance.stratification$q.table[,3,])

summaryStrataDifferences <- summary(strataDifferences)

summaryStrataDifferences


#write.csv(data.frame(summaryStrataDifferences), file="summary_strata_differences.csv")


# ----------
# obtain the summary of balance after stratification combining all strata

balance.stratification$sum.subclass

summary(abs(balance.stratification$sum.subclass$"Std. Mean Diff."))

table(abs(balance.stratification$sum.subclass$"Std. Mean Diff.") > 0.1)



# -->
# It can be concluded that adequate covariate balance was NOT achieved within any of the strata,
# using either the criterion that standardized mean differences should be less than 0.1 to be considered evidence of adequate balance
# or the less strict criterion of 0.25

# Therefore, treatment effect estimates obtained with these strata will have a substantial amount of bias.
# This bias could be due to a misspecified propensity score model or could be residual bias due to the limited number of strata
# and the fact that observations are not optimally allocated to strata to minimize bias.

# Increasing the number of strata will reduce residual bias, but having a larger number of strata may result in some of within-stratum group means becomes unreliable.

# The researcher may investigate the followin options to achieve adequate covariate balance:
# (1) Change the form of the propensity score model or the method to estimate propensity scores
# (2) remove within-stratum residual bias by combining stratification with weighting, matching or direct covariate adjustment with analysis of covariance (ANOVA)
#     or regression estimation of group means within strata
# (3) use marginal mean weighting through stratification





