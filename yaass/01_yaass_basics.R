setwd("//media//kswada//MyFiles//R//yaass")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  yaass
#   - dataset from the clinical area, contains 30 participants, of which 17 are of high-risk psychosis and 13 are healthy controls.
#     We have three metric variables pertaining to behavioral measures: affective empathy (AE), positive social experience (PSE), and perspective taking (PT).
#     Two additional measures come from fMRI scans (right-hand fRH and left/right foot fLRF).
#   - The variable scores are on the same scale (mean centered), but there are slight differencs in the sd's
# ------------------------------------------------------------------------------

data("yaass", package = "MPsychoR")

str(yaass)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------



