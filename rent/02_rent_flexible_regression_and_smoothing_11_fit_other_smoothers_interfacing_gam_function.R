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
# Interfacing with gam():  the ga() function
#   - ga() access to more than one-dimensional smoothers such as thin plate cubic splines (s()) and tensor products (tp()), by interfacing with gam(),
#     which are efficiently implemented within mgcv, in combination with a response variable distribution outside the exponential family,
#     and the capability for any or all parameters of the distribution to be modelled.
# ------------------------------------------------------------------------------

library(gamlss.add)


# ----------
# normal distribution
ga1 <- gam(R ~ s(Fl) + s(A), method = "REML", data = rent)

gn1 <- gamlss(R ~ ga(~ s(Fl) + s(A), method = "REML"), data = rent)

gb1 <- gamlss(R ~ pb(Fl) + pb(A), data = rent)


# Fitted global deviance
AIC(ga1, gn1, gb1, k = 0)


# -->
# For the normal models the fitted deviances are identical, but with slightly different degrees of freedom for the gam() model



# ----------
# gamma distribution

ga2 <- gam(R ~ s(Fl) + s(A), method = "REML", data = rent, family = Gamma(log))

gn2 <- gamlss(R ~ ga(~ s(Fl) + s(A), method = "REML"), data = rent, family = GA)

gb2 <- gamlss(R ~ pb(Fl) + pb(A), data = rent, family = GA)


# Fitted global deviance
AIC(ga2, gn2, gb2, k = 0)



# -->
# For the gamma modesl, gb2 gives slighly different results to the other two models.



# ----------
plot(ga2, scheme = 1)

term.plot(gn2)

term.plot(gb2)


# -->
# The top row shows the model fitted using gam(), the middle the model fitted using ga() within gamlss(),
# while the bottom row shows gamlss(9 using the pb() function.
# For all practical purposes the 3 plots lead to identical conclusions.



