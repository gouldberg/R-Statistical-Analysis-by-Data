setwd("//media//kswada//MyFiles//R//houseprices_us")

packages <- c("dplyr", "plm")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  House Prices US
# ------------------------------------------------------------------------------

data("HousePricesUS", package = "pder")


str(HousePricesUS)


dim(HousePricesUS)



# ------------------------------------------------------------------------------
# Tests for Serial Correlation:  Score test 
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Locally robust BSY tests for serial correlation or individual effects by pbsytest
#   - locally robust tests:  marginal tests with a correction that makes them robust to local deviations from the maintained hypothesis
#     The advantage of the robust tests is that the unconstrained model (RE-AR(1) for random effect model with first-order auto-regressive errors) need not be estimated
#   - This test can provide statistical evidence about the direction of misspecification in the (doubly) restricted model.
#     In fact, the presence of the "other" effect will not influence the test statistic as long as the magnitude is moderate.
#   - The robust tests allow us to discriminate between time-invariant error persistence (random effects) and time-decaying persistence (autoregressive errors)
#   - The test for random effects is implemented in the one-sided version, which takes into account that the variance of the random effect must be non-negative
# ------------------------------------------------------------------------------

php <- pdata.frame(HousePricesUS)


# ----------
pdim(php)


index(php)



# ----------
php.eq <- log(price) ~ log(income)



# ----------
bsy.LM <- matrix(ncol = 3, nrow = 2)


tests <- c("J", "RE", "AR")

dimnames(bsy.LM) <- list(c("LM test", "p-value"), tests)

for(i in tests){
  mytest <- pbsytest(php.eq, data = php, test = i)
  bsy.LM[1:2, i] <- c(mytest$statistic, mytest$p.value)
}


round(bsy.LM, 6)


# -->
# ??


