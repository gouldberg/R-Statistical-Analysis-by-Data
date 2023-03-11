setwd("//media//kswada//MyFiles//R//rep_vict")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Repeat victimization
#  - 8 * 8 table (from Fienberg) on repeat victimization for various crimes among respondents to a U.S. National Crime Survey.
#  - A special feature of this data set is that row and column categories reflect the same crimes, so substantial association is expected
# ------------------------------------------------------------------------------
data("RepVict", package = "vcd")

data <- RepVict

data


# For using argument "labeling_args"
names(dimnames(data)) <- c("FirstVictimization", "SecondVictimization")


