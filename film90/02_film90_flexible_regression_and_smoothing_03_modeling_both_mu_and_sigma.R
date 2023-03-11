setwd("//media//kswada//MyFiles//R//film90")

packages <- c("dplyr", "gamlss", "gamlss.add")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  film90
# ------------------------------------------------------------------------------
data("film90", package = "gamlss.data")


str(film90)

car::some(film90)



# ------------------------------------------------------------------------------
# Modeling both mu and sigma
# ------------------------------------------------------------------------------

m3 <- gamlss(lborev1 ~ pb(lboopen), sigma.formula = ~ pb(lboopen), data = film90, family = NO)



# ----------
# obtain the effective degrees of freedom for all the distribution parameters
edfAll(m3)



# ----------
plot(lborev1 ~ lboopen, col = "lightblue", data = film90)
lines(fitted(m1)[order(film90$lboopen)] ~ film90$lboopen[order(film90$lboopen)], col = "black", lty = 2)
lines(fitted(m3)[order(film90$lboopen)] ~ film90$lboopen[order(film90$lboopen)], col = "blue", lty = 1)


