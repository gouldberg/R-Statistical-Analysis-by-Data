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
# Fit the Box-Cox Cole and Green (BCCG) distribution: a three parameter continuous distribution
# ------------------------------------------------------------------------------

m5 <- gamlss(lborev1 ~ pb(lboopen), sigma.formula = ~ pb(lboopen), nu.formula = ~ pb(lboopen), data = film90, family = BCCG)



# ----------
# obtain the effective degrees of freedom for all the distribution parameters
edfAll(m5)



# ------------------------------------------------------------------------------
# Fit the Box-Cox power exponential (BCPE) distribution: 4 parameter continuous distribution
# ------------------------------------------------------------------------------

m6 <- gamlss(lborev1 ~ pb(lboopen), sigma.formula = ~ pb(lboopen), nu.formula = ~ pb(lboopen), tau.formula = ~ pb(lboopen), 
             data = film90, family = BCPE)

# start.from = m5 to start the iterations from the previous fitted m5 model.
# m6 <- gamlss(lborev1 ~ pb(lboopen), sigma.formula = ~ pb(lboopen), nu.formula = ~ pb(lboopen), tau.formula = ~ pb(lboopen), 
#             data = film90, start.from = m5, family = BCPE)


# ----------
# compare the model
plot(lborev1 ~ lboopen, col = "lightblue", data = film90)
lines(fitted(m1)[order(film90$lboopen)] ~ film90$lboopen[order(film90$lboopen)], col = "black", lty = 1)
lines(fitted(m3)[order(film90$lboopen)] ~ film90$lboopen[order(film90$lboopen)], col = "black", lty = 2)
lines(fitted(m5)[order(film90$lboopen)] ~ film90$lboopen[order(film90$lboopen)], col = "blue", lty = 3)
lines(fitted(m6)[order(film90$lboopen)] ~ film90$lboopen[order(film90$lboopen)], col = "red", lty = 3)


