setwd("//media//kswada//MyFiles//R//haireye_color")

packages <- c("dplyr", "datasets", "vcd", "vcdExtra", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  HairEyeColor
#  - frequencies of hair color, eye color, and sex from 592 students in a statistical course (Snee's 1974 sample)
#  - Neither hair color nor eye color is considered a response in relation to the other; Our interest concerns whether an association exists between them.
#    Hair color and eye color have both been classified into 4 categories. Although the categories used are among the most common,
#    they are not the only categories possible.
# ------------------------------------------------------------------------------
data("HairEyeColor", package = "datasets")

data <- HairEyeColor

data



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
