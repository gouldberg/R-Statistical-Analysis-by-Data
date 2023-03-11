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
# Smooth surface fitting Interfacing with gam():  the ga() function
#   - Thin plate splines:  rotationally invariant and are well suited to explanatory variables which are spatial coordinates in two (or 3) dimensions
#   - Tensor product splines:  scale invariant and are suited to explanatory variables measured in different units
# ------------------------------------------------------------------------------

library(gamlss.add)



# ----------
# Use thin plate splines

ga4 <- gam(R ~ s(Fl, A), method = "REML", data = rent, family = Gamma(log))


gn4 <- gamlss(R ~ ga(~ s(Fl, A), method = "REML"), data = rent, family = GA)


AIC(ga4, gn4, k = 0)



# ----------
vis.gam(ga4)


vis.gam(getSmo(gn4))



# ----------
# Note that term.plot() produces a contour plot similar to the plot(getSmo(gn4))
term.plot(gn4)

plot(getSmo(gn4))



# ----------
# Tensor product smoothers the gam() function te() is used

ga5 <- gam(R ~ te(Fl, A), data = rent, family = Gamma(log))

gn5 <- gamlss(R ~ ga(~ te(Fl, A)), data = rent, family = GA)


AIC(ga5, gn5, k = 0)


vis.gam(ga5)

vis.gam(getSmo(gn5))


term.plot(gn5)



