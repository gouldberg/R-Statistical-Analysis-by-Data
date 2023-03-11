setwd("//media//kswada//MyFiles//R//aids")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  aids
# ------------------------------------------------------------------------------
data("aids", package = "gamlss.data")


str(aids)

car::some(aids)



# ------------------------------------------------------------------------------
# prediction
# ------------------------------------------------------------------------------

# Fitting a negative binomial tyoe I distribution
aids.1 <- gamlss(y ~ poly(x, 3) + qrt, family = NBI, data = aids)



# ----------
# Plot the aids data with the fitted mu
plot(aids$x, aids$y)
lines(aids$x, aids.1$mu.fv)



# ----------
# fitted predictor for mu
predict(aids.1)

predict(aids.1, parameter = "mu")

predict(aids.1, parameter = "mu", type = "link")



# ----------
# fitted parameter mu
predict(aids.1, type = "response")

aids.1$mu.fv



# ------------------------------------------------------------------------------
# Standard errors for prediction
# ------------------------------------------------------------------------------

# Fitted parameter mu and its approximate standard errors for the observations
paids.1 <- predict(aids.1, what = "mu", se.fit = TRUE, type = "response")

paids.1$fit

paids.1$se.fit



# ----------
# Fitted predictor for mu and its approximate standard errors for the observations
paids.2 <- predict(aids.1, what = "mu", se.fit = TRUE, type = "link")

paids.2$fit

paids.2$se.fit



# ------------------------------------------------------------------------------
# The contribution to the predictor from each of the terms in the model formula
# ------------------------------------------------------------------------------

paids.3 <- predict(aids.1, what = "mu", type = "terms")

paids.4 <- predict(aids.1, what = "mu", type = "terms", se.fit = TRUE)


paids.3

paids.4



# ------------------------------------------------------------------------------
# Example of prediction
# ------------------------------------------------------------------------------

mod1 <- gamlss(y ~ poly(x, 3) + qrt, family = NBI, data = aids)

mod2 <- gamlss(y ~ bs(x, 5) + qrt, family = NBI, data = aids)

mod3 <- gamlss(y ~ pb(x) + qrt, family = NBI, data = aids)

mod4 <- gamlss(y ~ cs(x, df = 7) + qrt, family = NBI, data = aids)


GAIC(mod1, mod2, mod3, mod4)



# ----------
# predict the values for the response variable for the new data
newaids <- data.frame(x = c(46, 47, 48), qrt = c(2, 3, 4))

( ap1 <- predict(mod1, what = "mu", newdata = newaids, type = "response") )

( ap2 <- predict(mod2, what = "mu", newdata = newaids, type = "response") )

( ap3 <- predict(mod3, what = "mu", newdata = newaids, type = "response") )

( ap4 <- predict(mod4, what = "mu", newdata = newaids, type = "response") )



# ----------
graphics.off()
par(mfrow=c(1,1))
plot(y ~ x, data = aids, xlim = c(0, 55), ylim = c(0, 600))
lines(fitted(mod1) ~ aids$x, col = 2)
lines(fitted(mod2) ~ aids$x, col = 3)
lines(fitted(mod3) ~ aids$x, col = 4)
lines(fitted(mod4) ~ aids$x, col = 6)


# ----------
# Extrapolation
lines(ap1 ~ newaids$x, col = 2)
points(ap1 ~ newaids$x, col = 2)
lines(ap2 ~ newaids$x, col = 3)
points(ap2 ~ newaids$x, col = 3)
lines(ap3 ~ newaids$x, col = 4)
points(ap3 ~ newaids$x, col = 4)
lines(ap4 ~ newaids$x, col = 6)
points(ap4 ~ newaids$x, col = 6)


