setwd("//media//kswada//MyFiles//R//wenchuan")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Wenchuan
#   - dataset from clinical psychology area. McNally et al. (2015) collected data on PTSD (post-traumatic stress disorder) symptoms
#     reported by survivors of the Wenchuan earthquake in China using the PTSD checklist-civilian.
#     In total, there are 17 PTSD symptom items scaled on a 5-point rating scale (1 ... "not at all"; 5 ... "extremely").
# ------------------------------------------------------------------------------

data("Wenchuan", package = "MPsychoR")

str(Wenchuan)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

psych::describe(Wenchuan)

