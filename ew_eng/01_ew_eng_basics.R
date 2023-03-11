setwd("//media//kswada//MyFiles//R//ew_eng")

packages <- c("dplyr", "smacof")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  EW_eng
#   - Study of Borg and Braun (1996). They were interested in the difference between East Germans and West Germans in their work values shortly
#     after Germany reunited in 1990.
#     The items asked the respondents to rate 13 aspects of their work life (such as "high income" or "good changes for advancement")
#     on a scale from "not important" to "very important" to them personally.
# ------------------------------------------------------------------------------

data(EW_eng, package = "smacof")


str(EW_eng)


EW_eng$east

EW_eng$west



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------


