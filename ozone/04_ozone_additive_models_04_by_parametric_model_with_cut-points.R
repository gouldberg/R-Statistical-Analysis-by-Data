setwd("//media//kswada//MyFiles//R//ozone")

packages <- c("dplyr", "faraway")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  ozone
# ------------------------------------------------------------------------------

data(ozone, package="faraway")

str(ozone)



# ------------------------------------------------------------------------------
# Fit a parametric model using cutpoints
#   - use the fitted functions resulted from GAM model to help us find suitable simple transformations of the predictors
# ------------------------------------------------------------------------------

rhs <- function(x, c) ifelse(x > c, x - c, 0)
lhs <- function(x, c) ifelse(x < c, c - x, 0)

olm2 <- lm(O3 ~ rhs(temp, 60) + lhs(temp, 60) + rhs(ibh, 1000) + lhs(ibh, 1000), data = ozone)

summary(olm2)


# -->
# The fit is better and about as good as the additive model.
# The linear model has the advantage that we can write the prediction formula in a compact form.



# ------------------------------------------------------------------------------
# for reference
# ------------------------------------------------------------------------------

# use full dataset

amred <- gam(O3 ~ s(vh) + s(wind) + s(humidity) + s(temp) + s(dpg) + s(vis) + s(doy), data = ozone)

summary(amred)


# -->
# good fit with an R^2 = 80.5%, but at the cost of using effectively 19.4 (sum of the df) parameters including the intercept.



# ----------
# the linear model with all insignificant terms removed

alm <- lm(O3 ~ vis + doy + ibt + humidity + temp, data = ozone)

summary(alm)


# -->
# fit is substantially worse, but uses only 6 parameters.


