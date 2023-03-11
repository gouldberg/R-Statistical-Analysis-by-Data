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
# Nonparametric bootstrap (type3)
#   - In the nonparametric bootstrap, the original y and x values are resampled with replacement, and the original model is refitted to the new data
# ------------------------------------------------------------------------------

library(boot)


abd.funC <- function(data, i){
  d <- data[i,]

  coef(update(fit3, data = d))

}


( abd.T3 <- boot(abdomB, abd.funC, R = 999) )

abd.T3



# ----------
# plot the cubic (4th) parameter and get confidence intervals
plot(abd.T3, index = 4)

boot.ci(abd.T3, index = c(4, 1))



# ----------
# You can use the function histSmo() to obtain probabilities from the fitted distribution of the cubic coeffs.

FF <- histSmo(abd.T3$t[,4], plot = T)

1 - FF$cdf(20)

FF$cdf(20) - FF$cdf(-20)



