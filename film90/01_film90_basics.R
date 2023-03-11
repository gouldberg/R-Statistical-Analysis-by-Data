setwd("//media//kswada//MyFiles//R//film90")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  film90
#  - The original data were analyzed in Voudouris et al. (2012)
#  - variable:
#      - lnosc: the log of the number of screens in which the film was played
#      - lboopen: the log of box office opening week revenues
#      - lborev1: the log of box office revenues after the 1st week (the response variable which has been randomized)
#      - dist: a factor indicating whether the distributor of the film was an "Independent" or a "Major" distributor
# ------------------------------------------------------------------------------
data("film90", package = "gamlss.data")


str(film90)

car::some(film90)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

plot(lborev1 ~ lboopen, data = film90, col = "lightblue", xlab = "log opening revenue", ylab = "log extra revenue")



# ----------
car::scatterplot(lborev1 ~ lboopen, data = film90)

