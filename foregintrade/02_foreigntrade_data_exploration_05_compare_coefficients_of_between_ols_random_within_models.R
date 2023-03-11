setwd("//media//kswada//MyFiles//R//foreigntrade")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Foreign Trade
# ------------------------------------------------------------------------------

data("ForeignTrade", package = "pder")


str(ForeignTrade)


dim(ForeignTrade)


car::some(ForeignTrade)



# ----------
FT <- pdata.frame(ForeignTrade)



# ------------------------------------------------------------------------------
# Compare coefficients of four models
# ------------------------------------------------------------------------------

models <- c("within", "random", "pooling", "between")


sapply(models, function(x) coef(plm(imports ~ gnp, data = FT, model = x))["gnp"])



# -->
# "within" > "random" >> "pooling" > "between"