setwd("//media//kswada//MyFiles//R//ssocs")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Define survey design
# ------------------------------------------------------------------------------

library(survey)

surveyDesign <- svydesign(ids = ~0, strata = ~STRATA, weights = ~FINALWGT, data = SSOCS.data)



# ----------
names(surveyDesign)


