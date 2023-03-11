setwd("//media//kswada//MyFiles//R//tobinq")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  TobinQ
# ------------------------------------------------------------------------------

data("TobinQ", package = "pder")


str(TobinQ)

dim(TobinQ)



# ----------
pTobinQ <- pdata.frame(TobinQ)



# ------------------------------------------------------------------------------
# Compare coefficients of four models
# ------------------------------------------------------------------------------

models <- c("within", "random", "pooling", "between")


sapply(models, function(x) coef(plm(ikn ~ qn, data = pTobinQ, model = x))["qn"])



# -->
# Note that values of all coefficients are very small and close ... 