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
# Parametric bootstrap (type1): simulating the response from a fitted model
#   - Given model fit3 and its fitted mu and sigma values, simulate 999 bootstrapping samples for y from a logistic distribution.
#     Refit those new y's to the original x's using the model given by fit3  and check the distribution of the coeffs of the cubic term in the mu model
# ------------------------------------------------------------------------------

library(boot)


# A new data.frame with fitted mu and sigma
abdomA <- data.frame(abdom, fmu = fitted(fit3), fsigma = fitted(fit3, "sigma"))



# ----------
# function to save the mu coefficients

abd.funA <- function(data, i){
  d <- data

  # omit the first
  d$y <- if("origianl" %in% as.character(sys.call()))  d$y

    # simulate y
    else rLO(dim(d)[1], mu = d$fmu, sigma = d$fsigma)

  # fit and get the coef for mu
  coef(update(fit3, data = d))
}


( abd.T1 <- boot(abdomA, abd.funA, R = 999) )

abd.T1



# ----------
# plot the cubic (4th) parameter and get confidence intervals
plot(abd.T1, index = 4)

boot.ci(abd.T1, index = c(4, 1))


