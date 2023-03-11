setwd("//media//kswada//MyFiles//R//troutegg")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  troutegg
# ------------------------------------------------------------------------------

data("troutegg", package = "faraway")

str(troutegg)

head(troutegg)



# ------------------------------------------------------------------------------
# Logistic regression for binomial (proportion) responses
# ------------------------------------------------------------------------------

bmod <- glm(cbind(survive, total - survive) ~ location + period, family = binomial, troutegg)

summary(bmod)

faraway::sumary(bmod)




# ------------------------------------------------------------------------------
# Dispersion parameter
# ------------------------------------------------------------------------------

( sigma2 <- sum(residuals(bmod, type = "pearson")^2/bmod$df.residual) )


# -->
# We see that this is substantially larger than one as it would be in the standard binomial GLM.



# ------------------------------------------------------------------------------
# Quasi-Binomial GLM
# ------------------------------------------------------------------------------
# We can now make F-tests on the predictors
# "scale" argument using the estimated value of sigma2.
# If this argument is omitted, the deviance will be used in the estimation of the dispersion parameter.
# For this particular dataset, it makes very little difference, but in some cases, using the deviance to estimate the dispersion gives inconsistent results.

drop1(bmod, scale = sigma2, test = "F")

drop1(bmod, test = "F")



# -->
# We see that both terms are clearly significant.


# ----------
# No goodness of fit test is possible because we have a free dispersion parameter.
# We can use the dispersion parameter to scale up the estimates of the standard error
summary(bmod)
summary(bmod, dispersion = sigma2)

sumary(bmod)
sumary(bmod, dispersion = sigma2)


# -->
# We see that the differences in the location become less pronounced with only the fifth location being clearly different.



