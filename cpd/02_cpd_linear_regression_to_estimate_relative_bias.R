setwd("//media//kswada//MyFiles//R//cpd")

packages <- c("dplyr", "faraway")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  cpd
# ------------------------------------------------------------------------------

data(cpd, package="faraway")

str(cpd)

car::some(cpd)



# ------------------------------------------------------------------------------
# linear regression model to estimate relative bias in the projected sales
# ------------------------------------------------------------------------------

lmod <- lm(actual ~ projected - 1, data = cpd)

summary(lmod)


# -->
# beta:  The relative bias in the projected sales = 0.99402


# ----------
plot(actual ~ projected, cpd)
abline(lmod)



# ----------
par(mfrow=c(2,2))
plot(lmod)