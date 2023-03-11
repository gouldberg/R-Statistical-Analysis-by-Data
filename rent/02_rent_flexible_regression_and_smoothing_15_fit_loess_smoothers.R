setwd("//media//kswada//MyFiles//R//rent")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  rent
# ------------------------------------------------------------------------------
data("rent", package = "gamlss.data")


str(rent)

car::some(rent)



# ------------------------------------------------------------------------------
# Fit loess smoothers interfacing with loess():  the lo() function
#   - Note that in loess(), it is recommended that no more than 4 continuous variables should be used for fitting surfaces since
#     results may be unreliable for more variables.
#   - vis.lo() only works up to 2 explanatory variables
# ------------------------------------------------------------------------------

library(gamlss.add)


# additive model
r1 <- gamlss(R ~ lo(~ Fl) + lo(~A), data = rent, family = GA)


# smooth surface interaction model
r2 <- gamlss(R ~ lo(~ Fl * A), data = rent, family = GA)



AIC(r1, r2)



# ----------
# term.plot() colls vis.lo() to plot() the fitted model for the predictor of the distribution parameter (in this case mu)
term.plot(r1)


# more options int the plot by vis.lo()
vis.lo(getSmo(r1, which = 1), partial = T)

vis.lo(getSmo(r1, which = 2), partial = T)



# ----------
# The fitted interaction smooth surface
term.plot(r2, pages = 1)


# an approximate 95% CI surface and partial residuals
vis.lo(getSmo(r2, which = 1), se = 1.97, main = "(a) 95% CI")

vis.lo(getSmo(r2, which = 1), partial.res = T, main = "(b) partial residuals")



