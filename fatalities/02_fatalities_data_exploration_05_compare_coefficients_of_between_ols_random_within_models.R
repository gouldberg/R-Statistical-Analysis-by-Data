setwd("//media//kswada//MyFiles//R//fatalities")

packages <- c("dplyr", "AER")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Fatalities
# ------------------------------------------------------------------------------

data("Fatalities", packages = "AER")


str(Fatalities)

dim(Fatalities)



# ----------
Fatalities$frate <- with(Fatalities, fatal / pop * 10000)



# ----------
Fa <- pdata.frame(Fatalities)



# ------------------------------------------------------------------------------
# Compare coefficients of four models
# ------------------------------------------------------------------------------

models <- c("within", "random", "pooling", "between")


sapply(models, function(x) coef(plm(frate ~ beertax, data = Fa, model = x))["beertax"])



# -->
# OLS estimator and between estimator are close to each other
# Note that GLS estimator and within estimator are negative value, while OLS and between estimator are positive value !!!
