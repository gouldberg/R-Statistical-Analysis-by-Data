setwd("//media//kswada//MyFiles//R//pashkam")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Pashkam
#   - data derived from an fMRI experiment on goal-directed visual processing (Vaziri-Pashkam and Xu, 2017).
#     In this dataset the following eight objects were presented to participants:
#       - body (BD), cat(CT), chair (CH), car (CR), elephant (EL), face (FA), house (HO), and scissors (SC).
#   - The experiment involved three experimental conditions (color on objects and background, color on dots, color on objects),
#     three brain regions of interest, and two tasks (color and shape).
# ------------------------------------------------------------------------------

data("Pashkam", package = "MPsychoR")

str(Pashkam)


Pashkam



# ------------------------------------------------------------------------------
# basic analysis
# ------------------------------------------------------------------------------

psych::describe(Pashkam$color)

psych::describe(Pashkam$shape)
