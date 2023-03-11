setwd("//media//kswada//MyFiles//R//penicillin")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  penicillin
#  - Experiment to compare four processes, A, B, C, D, for the production of penicillin. These are the treatments.
#    The raw material, corn steep liquor, is quite variable and can only be made in blends sufficient for four runs.
#    Thus a randomized complete block design is suggested by the nature of the experimental units.
# ------------------------------------------------------------------------------
data("penicillin", package = "faraway")

str(penicillin)

car::some(penicillin)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

summary(penicillin)


xtabs(~ treat + blend, data = penicillin)



# ----------
library(lattice)
xyplot(yield ~ treat | blend, data = penicillin)




# ----------
library(ggplot2)
ggplot(penicillin, aes(x = treat, y = yield, shape = blend)) + geom_point() + xlab("Treatment")

ggplot(penicillin, aes(x = blend, y = yield, shape = treat)) + geom_point() + xlab("Blend")



# ----------
# calculate group mean
aggregate(penicillin$yield, by = list(penicillin$treat), FUN = "mean")

aggregate(penicillin$yield, by = list(penicillin$blend), FUN = "mean")


