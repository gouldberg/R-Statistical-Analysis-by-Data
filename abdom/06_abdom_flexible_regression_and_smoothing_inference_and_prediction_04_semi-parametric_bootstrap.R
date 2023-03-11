setwd("//media//kswada//MyFiles//R//abdom")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  abdom
# ------------------------------------------------------------------------------
data("abdom", package = "gamlss.data")


str(abdom)

car::some(abdom)



# ------------------------------------------------------------------------------
# Fit a logistic distribution
# ------------------------------------------------------------------------------

# Fit a logistic distribution with a parametric quadratic and cubic polynomial term for x in the mu model,
# and a linear term for x in the sigma model

fit2 <- gamlss(y ~ poly(x, 2), sigma.fo = ~x, data = abdom, family = LO)

fit3 <- gamlss(y ~ poly(x, 3), sigma.fo = ~x, data = abdom, family = LO)



# ----------
AIC(fit2, fit3)

LR.test(fit2, fit3)


# -->
# The AIC and the likelihood ratio test indicate that the quadratic model is superior to the cubic one.
# Now confirm this result using bootstrapping methods.



# ------------------------------------------------------------------------------
# Semi-Parametric bootstrap (type2)
#   - You sample from the residuals of the fitted model.
#     The residuals and the fitted values for mu and sigma need to be saved.
#   - The new simulated y's are taken by permuting the residuals, transforming them using the cdf of the normal distribution
#     (since they are normalized) to values between 0 and 1, and then using the inverse cdf of the logistic distribution
# ------------------------------------------------------------------------------

library(boot)


# ----------
abdomB <- data.frame(abdom, res = resid(fit3), fmu = fitted(fit3), fsigma = fitted(fit3, "sigma"))



# ----------
# function to save the mu coefficients

abd.funB <- function(data, i){
  d <- data

  # pNO is always the same for normalized residuals
  # the q function changes acoording to the family we fit
  d$y <- qLO(pNO(d$res[i]), mu = d$fmu, sigma =  d$fsigma)

  coef(update(fit3, data = d))

}


( abd.T2 <- boot(abdomB, abd.funB, R = 999) )

abd.T2



# ----------
# plot the cubic (4th) parameter and get confidence intervals
plot(abd.T2, index = 4)

boot.ci(abd.T2, index = c(4, 1))










