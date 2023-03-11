setwd("//media//kswada//MyFiles//R//species")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  species
#  - The data are viven and analysed by Stein and Juritz using a Poisson inverse Gaussian distribution for fish, with a linear model in log(lake) for log mu
#    and constant for sigma
#  - variables:
#       - fish:  the number of different species in 70 lakes in the world
#       - lake:  the lake area
# ------------------------------------------------------------------------------
data("species", package = "gamlss.data")


str(species)

car::some(species)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))
plot(log(fish) ~ log(lake), data = species, col = gray(0.7), pch = 15, cex = 0.5)


car::scatterplot(fish ~ log(lake), data = species)

car::scatterplot(log(fish) ~ log(lake), data = species)

