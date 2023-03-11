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
# itemfit check by computing a X^2-based itemfit statistic called Q1
#   - In general, a significant p-value suggests that the item does not fit.
#     But based on the fact that the Q1 statistic exhibits inflated Type I error rates,
#     we do not have to use these p-values as our only criterion to keep or eliminate items.
# ------------------------------------------------------------------------------

item.fit(fit2pl2)


# -->
# The results suggest that the entire set of items fits; none of the p-values is significant.
# Thus, no further item elimination steps are needed.


