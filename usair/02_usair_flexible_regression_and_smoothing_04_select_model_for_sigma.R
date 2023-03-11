setwd("//media//kswada//MyFiles//R//usair")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  usair
# ------------------------------------------------------------------------------
data("usair", package = "gamlss.data")


str(usair)

car::some(usair)



# ------------------------------------------------------------------------------
# Selecting a model for sigma
# ------------------------------------------------------------------------------

# Fit gamma distribution with all variables
mod1 <- gamlss(y ~ ., data = usair, family = GA)



# ----------
# Note that with only 41 observations and with a reasonably complicated model for mu,
# it is not advisable to attemp to fit smoothing terms for sigma.

mod4 <- stepGAIC(mod1, parameter = "sigma", scope = ~x1 + x2 + x3 + x4 + x5 + x6, k = log(41))

mod4$anova
