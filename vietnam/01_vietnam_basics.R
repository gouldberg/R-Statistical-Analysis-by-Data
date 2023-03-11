setwd("//media//kswada//MyFiles//R//vietnam")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS", "gpairs")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Vietnam
#  - 2 * 5 * 4 contingency table in frequency form reflecting a survey of student opinion on the Vietnam War at the University of North Carolina in May 1967.
#  - The table variables are sex, year in school, and response
#  - "response" variable
#     - (A) Defeat North Vietnam by widespread bombing and land invasion
#     - (B) Maintain the present policy
#     - (C) De-escalate military activity, stop bombing and begin negotiations
#     - (D) Withdraw military forces immediately
#
#  - The main interest is: How does the chosen response vary with sex and year ?
# ------------------------------------------------------------------------------

data("Vietnam", package = "vcdExtra")

dim(Vietnam)
str(Vietnam)


car::some(Vietnam)





