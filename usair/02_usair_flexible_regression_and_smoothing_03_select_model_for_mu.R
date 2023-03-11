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
# Selecting a model for mu by backward elimination
# ------------------------------------------------------------------------------

# Fit gamma distribution with all variables
mod1 <- gamlss(y ~ ., data = usair, family = GA)


mod2 <- stepGAIC(mod1)

mod21 <- stepGAIC(mod1, k = log(length(usair$y)))



# ----------
# anova shows the steps taken in the search of the model
mod2$anova

mod21$anova



# ----------
# selecting a model considering 2-way interaction terms
# the simplest model is the intercept, i.e., lower = ~1, and the most complicated is 2-way interactions

# Interactions of higher order than 2-waay are not permitted for continuous variables
mod3 <- stepGAIC(mod1, scope = list(lower = ~1, upper = ~(x1 + x2 + x3 + x4 + x5 + x6)^2), k = log(41))


mod3$anova



# ----------
# all the linear main effects and second-order interations plus smooth functions of the explanaotry variables
FORM <- as.formula("~ (X1 + x2 + x3 + x4 + x5 + x6)^2 + pb(x) + pb(x2) + pb(x3) + pb(x4) + pb(x5) + pb(x6)")

mod10 <- stepGAIC(mod1, scope = list(lower = ~1, uppter = FORM), k = log(41))

mod10$anova


# -->
# Note that, when using pb(), the linear part of the explanatory variable is automatically fitted separately, hence in stepGAIC() interactions
# involving the linear component of smoothing terms never enter into consideration.
# This is a limitation of stepGAIC(),


