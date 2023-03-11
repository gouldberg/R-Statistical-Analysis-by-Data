setwd("//media//kswada//MyFiles//R//psid")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  psid
# ------------------------------------------------------------------------------
data("psid", package = "faraway")

str(psid)

car::some(psid)



# ------------------------------------------------------------------------------
# Test the random effect terms for significance by parametric bootstrap
# ------------------------------------------------------------------------------

confint(mmod, method = "boot")



# -->
# We see that all the standard deviations are clearly well above zero.


