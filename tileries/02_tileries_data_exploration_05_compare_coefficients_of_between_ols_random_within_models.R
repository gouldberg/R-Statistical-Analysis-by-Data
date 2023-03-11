setwd("//media//kswada//MyFiles//R//tileries")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Tileries
# ------------------------------------------------------------------------------

data("Tileries", package = "pder")


str(Tileries)


dim(Tileries)


car::some(Tileries)



# ----------
Tl.p <- pdata.frame(Tileries)



# ------------------------------------------------------------------------------
# Compare coefficients of four models
# ------------------------------------------------------------------------------

models <- c("within", "random", "pooling", "between")

Tl.eq <- log(output) ~ log(labor) + log(machine)

sapply(models, function(x) coef(plm(Tl.eq, data = Tl.p, model = x))["log(labor)"])

sapply(models, function(x) coef(plm(Tl.eq, data = Tl.p, model = x))["log(machine)"])

