setwd("//media//kswada//MyFiles//R//rwdq")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  RWDQ (Work Design Questionnaire R Package Authors)
# ------------------------------------------------------------------------------

data("RWDQ", package = "MPsychoR")

str(RWDQ)


car::some(RWDQ)



# ------------------------------------------------------------------------------
# Item Characteristic Curves (ICCs)
# ------------------------------------------------------------------------------

# ICCs for the first five items
plot(fit2pl2, item = 1:5, legend = TRUE)



# -->
# 2-PL ICCs are allowed to cross.
# We see that wdq_25 has the largest discrimination parameter, leading to the steepest ICC slope.



# ------------------------------------------------------------------------------
# Discrimination parameters
# ------------------------------------------------------------------------------

sort(round(coef(fit2pl2)[, 2], 3), decreasing = TRUE)



# -->
# "Good" discrimination parameters are typically in the range from 0.8 to 2.5.
# Outside this range, the ICCs are either too flat (such items do not discriminate well along the entire continuum)
# or too steep (such items discriminate only within a very narrow trait range)

# wdq_257s discrimination parameters = 1.561


