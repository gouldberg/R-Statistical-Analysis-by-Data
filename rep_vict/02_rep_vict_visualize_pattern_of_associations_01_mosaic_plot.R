setwd("//media//kswada//MyFiles//R//rep_vict")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Repeat victimization
# ------------------------------------------------------------------------------
data("RepVict", package = "vcd")

data <- RepVict

data


# For using argument "labeling_args"
names(dimnames(data)) <- c("FirstVictimization", "SecondVictimization")



# ------------------------------------------------------------------------------
# mosaic plot
# ------------------------------------------------------------------------------
mosaic(data, gp = shading_Friendly2, labeling_args = list(abbreviate_labs = c(FirstVictimization = 7, SecondVictimization = 7), rot_labels = c(left = -45)))

