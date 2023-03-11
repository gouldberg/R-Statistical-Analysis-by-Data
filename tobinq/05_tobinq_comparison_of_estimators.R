setwd("//media//kswada//MyFiles//R//tobinq")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  TobinQ
# ------------------------------------------------------------------------------

data("TobinQ", package = "pder")

str(TobinQ)

dim(TobinQ)




# ------------------------------------------------------------------------------
# Comparison of estimators
# ------------------------------------------------------------------------------

sapply(list(pooling = Q.pooling, within = Q.within, between = Q.between, swar = Q.swar),
       function(x) coef(summary(x))["qn", c("Estimate", "Std. Error")])



# -->
# We can expect the OLS (pooling) and GLS (random) estimators to give intermediate results between the within and the between estimators
# as they use both sources of variance.
# GLS estimator, the weights depend not only on the shares of the variance of the covariates but also on the variance of the errors.
# GLS estimator will always give less weight to the between variance, as theta < 1.

# theta --> 0:  this means that sigma(v) is small compared to sigma(eta), GLS estimator converges to the within estimator
# theta --> 1:  this means that sigma(v) is large compared to sigma(eta), GLS estimator converges to the OLS estimator


# -->
# Here, the OLS and GLS estimators are in the interval defined by the within and between estimators
# and the GLS estimator is close to the within estimator than OLS.

# For standard deviations, OLS seems to be the most efficient model, but remember that the standard formula for computing
# the variance of the OLS estimator is biased if individual effects are present.
# The standard deviation for the GLS estimator is slightly lower than for the within estimator and much lower than for the between estimator.



# ------------------------------------------------------------------------------
# Share of the variances for the covariate
# ------------------------------------------------------------------------------

summary(pTobinQ$qn)



# ------------------------------------------------------------------------------
# OLS estimator
# ------------------------------------------------------------------------------

SxxW <- sum(Within(pTobinQ$qn) ^ 2)

SxxB <- sum((Between(pTobinQ$qn) - mean(pTobinQ$qn)) ^ 2)

SxxTot <- sum((pTobinQ$qn - mean(pTobinQ$qn)) ^ 2)


# The weight of Within model
( pondW <- SxxW / SxxTot )



# ----------
# OLS estimator 
pondW * coef(Q.within)[["qn"]] + (1 - pondW) * coef(Q.between)[["qn"]] 



# ------------------------------------------------------------------------------
# GLS estimator
# ------------------------------------------------------------------------------

T <- 35

N <- 188

smtx2 <- deviance(Q.between) * T / (N - 2)

sidios2 <- deviance(Q.within) / (N * (T - 1) - 1)

( phi <- sqrt(sidios2 / smtx2) )


# The weight of Within estimator for GLS estimaator
( pondW  <- SxxW / (SxxW + phi ^ 2 * SxxB) )



# ----------
# GSL estimator
pondW * coef(Q.within)[["qn"]] + (1 - pondW) * coef(Q.between)[["qn"]] 



# -->
# The weight of the within estimator is much larger for the GLS estimator than for the OLS estimator.
# This is mainly due to the fact that T is large (35 years).
# The GLS estimator (0.039) is therefore very close to the within estimator (0.0038)



