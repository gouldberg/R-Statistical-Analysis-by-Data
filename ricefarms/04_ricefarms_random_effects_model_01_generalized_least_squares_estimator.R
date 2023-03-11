setwd("//media//kswada//MyFiles//R//ricefarms")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  RiceFarms
# ------------------------------------------------------------------------------

data("RiceFarms", package = "splm")


str(RiceFarms)


dim(RiceFarms)



# ------------------------------------------------------------------------------
# Random Effects Model
#   - Fixed effects model is appropriate when the unobserved heterogeneity term is correlated with the independent variables.
#   - When this is not the case, we can use either OLS, or, when OLS is not enough, the random effects model
# ------------------------------------------------------------------------------

Rice <- pdata.frame(RiceFarms, index = "id")


# ----------
pdim(Rice)


index(Rice)



# ----------
rice.eq <- log(goutput) ~ log(seed) + log(totlabor) + log(size)


# Estimate the Swamy and Arora model
r.swar <- plm(rice.eq, data = Rice, model = "random", random.method = "swar")


# Same Swamy and Arora model
# random.models = c("within", "between"):  use the within residuals to estimate sigma(v) and the between residuals to estimate sigma(eta)
# random.dfcor = c(2,2):  indicating what is the denominator of the two quadratic forms
#  - 0: the number of observations is used (NT, N)
#  - 1: the numerators of the theoretical formulas are used (N(T - 1), N)
#  - 2: the number of estimated parameters are deduced (N(T - 1) - K, N - K - 1)

r.swar2 <- plm(rice.eq, data = Rice, model = "random", random.models = c("within", "between"), random.dfcor = c(2,2))



# ----------
summary(r.swar)




# -->
# theta = 0.225:  part of the individual mean that is removed from each variable for the GLS estimator = 22.5%
# This implies that the GLS estimator is closer to the OLS estimator (theta = 0) than to the Within estimator (theta = 1)



# ----------
# This can be obtained from ercomp()
ercomp(r.eq, Rice)

ercomp(r.eq, Rice)$theta

