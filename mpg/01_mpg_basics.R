setwd("//media//kswada//MyFiles//R//mpg")

packages <- c("dplyr", "lattice", "gamair")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# This tutorial is based on Chapter 7. Introductin GAMs from "Generalized Additive Models An Introduction with R 2nd Edition" (by Simon N. Wood)


# ------------------------------------------------------------------------------
# data:  mpg
#   - Contains data on fuel efficiency of cars along with characteristics of the cars
#     For each car thre is a city driving and highway driving fuel efficiency given in miles per gallon (the data are from the US).
#     It makes sense to treat the fuel efficiency measurements as a bivariate response.
#   - Variables (main only)
#        - fuel:  diesel or petrol (gasoline)
#        - style:  factor for style of car
#        - drive:  factor for front, rear or all wheel drive
#        - make:  factor for manufacturer  -- a random effect is assumed for thie
# ------------------------------------------------------------------------------

data(mpg)

str(mpg)

dim(mpg)



# ------------------------------------------------------------------------------
# basics analysis
# ------------------------------------------------------------------------------

plot(mpg$city.mpg, mpg$hw.mpg, pch = ".", cex = 3)


