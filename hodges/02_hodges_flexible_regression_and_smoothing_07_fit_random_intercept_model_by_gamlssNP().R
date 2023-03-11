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
# random intercept model by gamlssNP()
#   - The function gamlssNP() uses "global" estimation and can apply Gaussian quadrature to approximate the marginal likelihood of the response variable Y
#     by replacing the integration over a normally distributed random effects variable in the predictor for mu by a summation.
#     This allows (global) estimation of the corresponding single random effects hyperparameter lambda by maximizing the marginal likelihood of the data.
#
#   - gamlssNP() can also fit (nonparametric) discrete random effects
# ------------------------------------------------------------------------------

library(gamlss.mx)


# Fitting a normal random intercept for mu with K = 10, 20, using Gaussian quadrature
mgq10 <- gamlssNP(prind ~ 1, K = 10, random = ~1 | state, mixture = "gq", data = hodges)


mgq20 <- gamlssNP(prind ~ 1, K = 20, random = ~1 | state, mixture = "gq", data = hodges)



# ----------
mgq10


mgq20



# ------------------------------------------------------------------------------
# Compare sigma_b with lme model
# ------------------------------------------------------------------------------

getSigmab(l1)

getSigmab(l2)

mgq10$mu.coefficients[2]

mgq20$mu.coefficients[2]



# ------------------------------------------------------------------------------
# Compare sigma_e with lme model
# ------------------------------------------------------------------------------

l1$sigma

l2$sigma


# --> BUT DO NOT KNOW FOR gamlss.NP() model ...



# ------------------------------------------------------------------------------
# Compare residuals with lme model
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow=c(2,2), mar = c(2,2,2,2))

plot(resid(mgq10))

plot(resid(mgq20))

plot(resid(l2, level = 1, type = "p"))



# ------------------------------------------------------------------------------
# Compare fitted values with lme model
# ------------------------------------------------------------------------------

# fitted value mgq10 vs mgq20
graphics.off()
par(mfrow=c(1,1))

plot(prind ~ unclass(state), data = hodges, pch = ".", cex = 2, col = gray(0.6))

points(mgq10$ebp ~ unclass(state), data = hodges, pch = 5, col = "red")

points(mgq20$ebp ~ unclass(state), data = hodges, pch = 5, col = "blue")

lines(fitted(m0), lty = 2, col = "gray")



# -->
# mgq20 has more shrinkage towards to overall mean



# ----------
# fitted value mgq20 vs lme model
graphics.off()
par(mfrow=c(1,1))

plot(prind ~ unclass(state), data = hodges, pch = ".", cex = 2, col = gray(0.6))

points(mgq20$ebp ~ unclass(state), data = hodges, pch = 5, col = "red")

points(fitted(l1, level = 1) ~ unclass(state), data = hodges, pch = 5, col = "green")

lines(fitted(m0), lty = 2, col = "gray")




