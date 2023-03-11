setwd("//media//kswada//MyFiles//R//jsp")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  jsp
# ------------------------------------------------------------------------------
data("jsp", package = "faraway")

str(jsp)

car::some(jsp)



# ----------
library(plm)

jsp.p <- pdata.frame(jsp, index = c("id", "year"))




# ------------------------------------------------------------------------------
# Compare coefficients of four models
# ------------------------------------------------------------------------------

models <- c("within", "random", "pooling", "between")


sapply(models, function(x) coef(plm(math ~ english, data = jsp.p, model = x))["english"])



# -->
# interestingly, within < random < pooling < between