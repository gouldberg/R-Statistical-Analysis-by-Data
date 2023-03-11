setwd("//media//kswada//MyFiles//R//hodges")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  hodges
# ------------------------------------------------------------------------------

data("hodges", package = "gamlss.data")


str(hodges)

car::some(hodges)



# ------------------------------------------------------------------------------
# Fit null and main effect model for the factor state
# ------------------------------------------------------------------------------

# null model (only the constant)
m0 <- gamlss(prind ~ 1, data = hodges)


# main effect model
m1 <- gamlss(prind ~ state, data = hodges)



summary(m0)

summary(m1)



# ----------
# plot the observations and the fitted values for both models

# plot(prind ~ state, data = hodges, pch  =3)
plot(prind ~ unclass(state), data = hodges, pch  =3)

points(fitted(m1) ~ unclass(state), data = hodges, pch = 5, col = "red")

lines(fitted(m0))



# ----------
plot(m1)


wp(m1)

