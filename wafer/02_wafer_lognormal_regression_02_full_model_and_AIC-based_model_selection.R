setwd("//media//kswada//MyFiles//R//wafer")

packages <- c("dplyr", "faraway")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  wafer
# ------------------------------------------------------------------------------

data(wafer, package="faraway")

str(wafer)

car::some(wafer)



# ------------------------------------------------------------------------------
# Fit the full model and reduce it using AIC-based model selection
# ------------------------------------------------------------------------------

llmdl <- lm(log(resist) ~ .^2, wafer)

summary(llmdl)



# ----------
rlmdl <- step(llmdl, direction = "both")

summary(rlmdl)


# -->
# we find a model with 3 2-way interactions, all with x3
# a lognormal distribution with a small variance (residual standard error = 0.06735) is very well approximated by a normal



# ----------
plot(rlmdl, 1:2)

