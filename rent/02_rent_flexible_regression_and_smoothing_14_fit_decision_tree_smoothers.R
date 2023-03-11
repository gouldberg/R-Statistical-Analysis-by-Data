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
# Fit decision tree smoothers interfacing with rpart():  the tr() function
# ------------------------------------------------------------------------------

library(gamlss.add)


r1 <- gamlss(R ~ tr(~ Fl + A + H + B + loc), data = rent, family = GA, gd.tol = 100)


r2 <- gamlss(R ~ tr(~ Fl + A + H + B + loc), sigma.fo = ~ tr(~ Fl + A + H + B + loc), data = rent, family = GA, gd.tol = 100, c.crit = 0.1)


AIC(r1, r2)



# ----------
# Visual representation for the mu parameter of the decision tree model
term.plot(r2, parameter = "mu", pages = 1)


# Visual representation for the sigma parameter of the decision tree model
plot(getSmo(r2, parameter = "sigma"))
text(getSmo(r2, parameter = "sigma"))
