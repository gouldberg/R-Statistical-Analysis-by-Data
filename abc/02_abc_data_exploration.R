setwd("//media//kswada//MyFiles//R//abc")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  ABC
# ------------------------------------------------------------------------------

data("ABC", package = "MPsychoR")

str(ABC)

dim(ABC)

car::some(ABC)



# ------------------------------------------------------------------------------
# basic analysis:  Variance of each variables
# ------------------------------------------------------------------------------

psych::describe(ABC)



# ------------------------------------------------------------------------------
# basic analysis:  pairwise-correlation
# ------------------------------------------------------------------------------

psych::pairs.panels(ABC)








