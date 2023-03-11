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
# random intercept model by random()
#   - The function random() is based on PQL methodology, and can be used like all other additive smoothing terms within GAMLSS to model any or
#     all of the parameters of the response distribution, using normal random effects.
#
#   - The function random() is based on the original function with the same name in the package gam.
#     In the gamlss version, the function has been modified to allow a "local" maximum likelihood estimation of the random effect parameter sigma_k.
#     This method is equivalent to the PQL method of Breslow and Clayton, applied at the local iterations of the algorithm.
#     (In fact for a GLM and a simple random effect, it is equivalent to glmmPQL() in the MASS package).
#
#   - The function allows the fitted values for a factor term to be shrunk towards the overall mean, where the amount of shrinking depends either
#     on the smoothing parameter lambda (lambda = sigma_e / sigma_b) or the effective degrees of freedom or on the sigma parameter (sigma_b, default, where sigma_b = sigma_k, here).
#
#   - Conditional AIC = -2 * log L + 2 * sigma(df_k)  (k = 1,2,3,4 for mu, sigma, nu, tau)
# ------------------------------------------------------------------------------

m2 <- gamlss(prind ~ random(state), data = hodges, trace = FALSE)


getSmo(m2)


plot(m2)



# ------------------------------------------------------------------------------
# random intercept model by re()
#   - The function re() is based on PQL methodology, and can be used like all other additive smoothing terms within GAMLSS to model any or
#     all of the parameters of the response distribution, using normal random effects.
#
#   - The function re() is an interface to the widely used function lme() of the nlme package.
#     re() can be used for normally distributed random intercepts and slopes, repeated measurements, multilevel modelling
#     and all other models implemented in lme(), as described by Pinheiro and Bates (2000)
#
#   - The function re() can be used for any or all the parameters of the distribution and therefore the following GAMLSS model is appropriate.
# ------------------------------------------------------------------------------

m3 <- gamlss(prind ~ re(random = ~1 | state), data = hodges, trace = FALSE)


getSmo(m3)


plot(m3)




# ------------------------------------------------------------------------------
# random intercept model by re() with method = "REML"
# ------------------------------------------------------------------------------

m31 <- gamlss(prind ~ re(random = ~1 | state, method = "REML"), data = hodges, trace = FALSE)


getSmo(m31)


plot(m31)




# ------------------------------------------------------------------------------
# Compare models
# ------------------------------------------------------------------------------

AIC(m2, m3, m31)


getSmo(m2)

getSmo(m3)

getSmo(m31)


# -->
# The estimate of sigma_b = 14.035 is identical for m2(random) and m3(re)
# but different for m31(re, "REML")

# Fitted degrees of freedom for mu are different



# ----------
# fitted values

plot(prind ~ unclass(state), data = hodges, pch = 3)

points(fitted(m1) ~ unclass(state), data = hodges, pch = 5, col = "red")

points(fitted(m2) ~ unclass(state), data = hodges, pch = 4, col = "blue")

# points(fitted(m3) ~ unclass(state), data = hodges, pch = 6, col = "green")

lines(fitted(m0), lty = 2, col = "gray")



# ------------------------------------------------------------------------------
# If the re() function is used within gamlss(), lme functions can be used
# ------------------------------------------------------------------------------

# summary
summary(getSmo(m3))


# residual level = 1 plot
plot(getSmo(m3))


# random effect estimates
ranef(getSmo(m3))


# fitted coefficients
coef(getSmo(m3))


# Confidence intervals
intervals(getSmo(m3))


# ANOVA
anova(getSmo(m3))






