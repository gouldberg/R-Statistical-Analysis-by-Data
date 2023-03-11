setwd("//media//kswada//MyFiles//R//yaass")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  yaass
# ------------------------------------------------------------------------------

data("yaass", package = "MPsychoR")

str(yaass)



# ------------------------------------------------------------------------------
# basic analysis:  Variance of each variables
# ------------------------------------------------------------------------------

# The variable scores are on the same scale (mean centered), but there are slight differences in the sd's
psych::describe(yaass)



# ------------------------------------------------------------------------------
# basic analysis:  pairwise-correlation
# ------------------------------------------------------------------------------

psych::pairs.panels(yaass)


# -->
# PSE vs AE, AE vs. PT are highly correlated.






