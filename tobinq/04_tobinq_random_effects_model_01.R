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
# Random Effects Model
#   - Fixed effects model is appropriate when the unobserved heterogeneity term is correlated with the independent variables.
#   - When this is not the case, we can use either OLS, or, when OLS is not enough, the random effects model
# ------------------------------------------------------------------------------

pTobinQ <- pdata.frame(TobinQ)


# ----------
pdim(pTobinQ)


index(pTobinQ)




# ----------
Qeq <- ikn ~ qn


# Estimate the Swamy and Arora model
Q.swar <- plm(Qeq, pTobinQ, model = "random", random.method = "swar")


# Same Swamy and Arora model
# random.models = c("within", "between"):  use the within residuals to estimate sigma(v) and the between residuals to estimate sigma(eta)
# random.dfcor = c(2,2):  indicating what is the denominator of the two quadratic forms
#  - 0: the number of observations is used (NT, N)
#  - 1: the numerators of the theoretical formulas are used (N(T - 1), N)
#  - 2: the number of estimated parameters are deduced (N(T - 1) - K, N - K - 1)

Q.swar2 <- plm(Qeq, pTobinQ, model = "random", random.models = c("within", "between"), random.dfcor = c(2,2))



# ----------
summary(Q.swar)




# -->
# theta = 0.735:  part of the individual mean that is removed from each variable for the GLS estimator = 73.5%
# This high value is due to the large time dimension of this panel (T = 35).
# This implies that the GLS estimator is closer to the within estimator (theta = 1) than to the OLS estimator (theta = 0)



# ----------
# This can be obtained from ercomp()
ercomp(Qeq, pTobinQ)

ercomp(Qeq, pTobinQ)$theta

