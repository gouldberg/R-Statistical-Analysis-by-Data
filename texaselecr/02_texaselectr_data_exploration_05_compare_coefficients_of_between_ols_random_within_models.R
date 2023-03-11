setwd("//media//kswada//MyFiles//R//texaselectr")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Texas Electr
#  - This data were used by Kumbhakar (1996) and Horrace and Schmidt (1996) and concern the production cost of electric firms in Texas.
# ------------------------------------------------------------------------------

data("TexasElectr", package = "pder")


str(TexasElectr)


dim(TexasElectr)


car::some(TexasElectr)



# ----------
TexasElectr$cost <- with(TexasElectr, explab + expfuel + expcap)



# ----------
TE <- pdata.frame(TexasElectr)



# ------------------------------------------------------------------------------
# Compare coefficients of four models
# ------------------------------------------------------------------------------

models <- c("within", "random", "pooling", "between")


sapply(models, function(x) coef(plm(log(cost) ~ log(output), data = TE, model = x))["log(output)"])



# -->
# The GLS estimator is close to the OLS estimator
# because the transformation removes about only 8% of the individual mean
