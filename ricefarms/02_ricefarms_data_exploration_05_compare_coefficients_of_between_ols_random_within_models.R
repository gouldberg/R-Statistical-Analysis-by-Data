setwd("//media//kswada//MyFiles//R//ricefarms")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  RiceFarms
# ------------------------------------------------------------------------------

data("RiceFarms", package = "splm")


str(RiceFarms)


dim(RiceFarms)



# ----------
Rice <- pdata.frame(RiceFarms, index = "id")



# ------------------------------------------------------------------------------
# Compare coefficients of four models
# ------------------------------------------------------------------------------

models <- c("within", "random", "pooling", "between")


rice.eq <- log(goutput) ~ log(seed) + log(totlabor) + log(size)

sapply(models, function(x) coef(plm(rice.eq, data = Rice, model = x))["log(seed)"])
sapply(models, function(x) coef(plm(rice.eq, data = Rice, model = x))["log(totlabor)"])
sapply(models, function(x) coef(plm(rice.eq, data = Rice, model = x))["log(size)"])



# -->
# values of all coefficients (within, GLS, OLS, between) are almost same ??!??