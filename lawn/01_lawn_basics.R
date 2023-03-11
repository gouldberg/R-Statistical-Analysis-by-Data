setwd("//media//kswada//MyFiles//R//lawn")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  lawn
#   - Data on the cutoff times of lawnmowers.
#     Three machines were randomly selected from those produced by manufacturers A and B.
#     Each machine was tested twice at low speed and high speed
# ------------------------------------------------------------------------------

data("lawn", package = "faraway")

str(lawn)


car::some(lawn)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
