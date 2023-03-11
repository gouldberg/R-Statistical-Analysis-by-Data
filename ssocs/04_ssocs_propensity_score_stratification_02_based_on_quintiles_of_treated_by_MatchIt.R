setwd("//media//kswada//MyFiles//R//ssocs")

packages <- c("dplyr", "MatchIt", "Matching", "survey")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Propensity score stratification only for the treated
# ------------------------------------------------------------------------------

# IF a researcher is interested in estimating the ATT, it may be preferable to stratify based on quintiles of the treated,
# so that each group has approximately the same number of treated cases,
# because better covariate balance nad power to test the treatment effect may be obtained than when stratification is based on the propensity scores of both groups


# evaluate covariate balance within strata separately for categorical and continuous variables
# MatchIt automates the creation of strata, strata weights, and evaluation of covariate balance
library(MatchIt)

SSOCS.data2 <- SSOCS.data[,c("SCHID", "treat","percHarsh", "pScores","pScores", "STRATA", "FINALWGT",covariateNames)]


balanceFormula <- paste(covariateNames, collapse = "+")

balanceFormula <- formula(paste("treat~", balanceFormula, sep = ""))


# here set propensity scores for distance
stratification <- matchit(balanceFormula, distance = SSOCS.data2$pScores, data = SSOCS.data2,  method = "subclass", sub.by = "treat", subclass = 5)



# ----------
data.stratification <- match.data(stratification)

data.stratification$treat <- factor(data.stratification$treat, levels = c(0,1), labels = c("Untreated","Treated"))

data.stratification$subclass <- factor(data.stratification$subclass)

xtabs(~ treat + subclass, data.stratification)



# -->
# In the cross-classification of treatment by strata, the stratification based on the propensity scores of the treated resulted in a similar
# number of treated units within strata.
# The differences in number of untreated units across strata indicate that the distribution of propensity scores for the untreated is positively skewed.
# Also, the cross-classification indicates that there is no common support problem, because there are some untreated units in all of the strata.

