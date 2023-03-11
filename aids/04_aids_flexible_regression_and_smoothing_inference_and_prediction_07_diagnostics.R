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
# Diagnostics
# ------------------------------------------------------------------------------

mod4 <- gamlss(y ~ cs(x, df = 7) + qrt, family = NBI, data = aids)


plot(mod4)

plot(mod4, ts = TRUE)



# ----------
wp(mod4)



# ----------
# rqres.plot() is used to create multiple realizations of the normalized randomized qunatile residuals when the distribution of the response
# variable is discrete.

rqres.plot(mod4)

rqres.plot(mod4, type = "QQ")



# ----------
Q.stats(resid = resid(mod4), xvar = aids$x, n.inter = 5)
# Q.stats(mod4, xvar = aids$x, n.inter = 5)


# nearly misfit (1.91, which is close to threshold 2.0)
# in the variance of the residuals in the fourth interval 27.5 to 36.5 is identified.