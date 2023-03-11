setwd("//media//kswada//MyFiles//R//els_career_academy")

packages <- c("dplyr", "mice", "mitools")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Estimate propensity score by logistic regression
# ------------------------------------------------------------------------------

# glm is not used here, because it does not provide estimation for complex survey samples.
# Instead, the survey package is used, because it provides estimation of logistic regression models with design-based adjustment for
# the effects of unequal-probability sampling, clustering, and stratification.


# fit a logistic regression model with adjustments for clustering and stratification to the first imputed dataset
# the argument family = quasibinomial is needed instead of family = binomial because there are sampling weights that cause overdispersion
psModel <- svyglm(psFormula, design = surveyDesign, family = quasibinomial)



# ----------
# extract propensity scores from first dataset
pScores <-  fitted(psModel)

SSOCS.data$pScores <-  pScores

