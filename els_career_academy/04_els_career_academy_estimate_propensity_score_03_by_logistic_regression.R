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
psModel1 <- svyglm(psFormula, design = surveyDesign1, family = quasibinomial)


# fit a logistic regression model to all imputed datasets
psModelAll <- with(surveyDesignAll, svyglm(psFormula, family = quasibinomial))



# ----------
# extract propensity scores from first dataset
pScores <-  fitted(psModel1)


imputation1$pScores <-  pScores




# ----------
# extract propensity scores from all imputed datasets
pScoresAll <- sapply(psModelAll, fitted)


# combine propensity scores across imputed datasets by taking the mean
pScoresMean <- apply(pScoresAll, 1, mean)


# add propensity score mean to imputed datasets
allImputations <- update(allImputations, pScores = pScoresMean)

