setwd("//media//kswada//MyFiles//R//crabs")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  crabs
# ------------------------------------------------------------------------------

data("crabs", package = "MASS")


str(crabs)


car::some(crabs)



# ------------------------------------------------------------------------------
# Estimate the slope and intercept parameters by major axis method
#   - Warton et al. have developed a full package (smatr) for estimating slopes and intercepts according to the major axis and reduced major axis methods
#     The major axis Method is preferred over regression because one can interpret the major axis as a size axis
# ------------------------------------------------------------------------------

library(smatr)


MRW <- crabs$RW[1:50]
FRW <- crabs$RW[51:100]
MFL <- crabs$FL[1:50]
FFL <- crabs$FL[51:100]



# ----------
# smatr::line.cis() estimates the slopes and the intercepts according to the reduced major axis, major axis, or ordinary least-squares methods.
# Compute major axis slopes and intercepts for males and females of the blue species

line.cis(log(FFL), log(FRW), method = "MA")

line.cis(log(MFL), log(MRW), method = "MA")


