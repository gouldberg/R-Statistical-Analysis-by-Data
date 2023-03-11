setwd("//media//kswada//MyFiles//R//ssocs")

packages <- c("dplyr", "MatchIt", "Matching", "survey")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Propensity score stratification
# ------------------------------------------------------------------------------

# divide the sample into strata based on propensity scores.
# Strata can be created by converting the propensity score variable into a categorical variable using thresholds defined by
# quantiles of the propensity score.

# The most common choice for number of strata is five.
# cut propensity scores into five strata for estimating the ATT

SSOCS.data$subclass <- cut(x = SSOCS.data$pScores, breaks = quantile(SSOCS.data$pScores, prob = seq(0, 1, 1/5)), include.lowest = T)


# rename the strata labels
levels(SSOCS.data$subclass) <- 1:length(levels(SSOCS.data$subclass))


# examine common support
xtabs(~ treat + subclass, SSOCS.data)


#save the data
#save(SSOCS.data, file="SSOCS_data_complete.Rdata", compress=T)


# -->
# In the cross-classification of treatment group by strata shown above, the total stratum size is approximately the same for all strata.
# The differences in stratum sizes indicate that hte distribution of propensity scores for the treated is negatively skewed,
# while the distribution for the untreated is positively skewed.

# Common support is adequate because in all strata, there are some treated and some untreated observations.


