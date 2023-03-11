setwd("//media//kswada//MyFiles//R//turtles")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  turtles
#   - data set of Jolicoeur and Mosimann
# ------------------------------------------------------------------------------

data("turtles", package = "Flury")


str(turtles)


car::some(turtles)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

