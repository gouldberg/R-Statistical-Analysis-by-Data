setwd("//media//kswada//MyFiles//R//kipt")

packages <- c("dplyr", "smacof")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  KIPT
#   - The inter-correlations of eight test items of the Kennedy Institute Phonics Test (KIPT), a test for reading skills.
# ------------------------------------------------------------------------------

data(KIPT, package = "smacof")


str(KIPT)


car::some(KIPT)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------


