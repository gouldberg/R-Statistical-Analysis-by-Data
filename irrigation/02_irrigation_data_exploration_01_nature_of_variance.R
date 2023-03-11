setwd("//media//kswada//MyFiles//R//irrigation")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  irrigation
# ------------------------------------------------------------------------------

data("irrigation", package = "faraway")

str(irrigation)

car::some(irrigation)




# ------------------------------------------------------------------------------
# data exploration:  nature of variance
# ------------------------------------------------------------------------------

summary(irrigation)


xtabs(~ field + variety, data = irrigation)



# ----------
library(lattice)

xyplot(yield ~ variety | field, data = irrigation)

xyplot(yield ~ field | variety, data = irrigation)




# ----------
library(ggplot2)

ggplot(irrigation, aes(x = field, y = yield, shape = irrigation, color = variety)) + geom_point(size = 4)



# -->
# The irrigation and variety are fixed effects, but the field is clearly a random effect.
# We must also consider the interaction between field and variety, which is necessarily also a random effect because one of the two components is random.


