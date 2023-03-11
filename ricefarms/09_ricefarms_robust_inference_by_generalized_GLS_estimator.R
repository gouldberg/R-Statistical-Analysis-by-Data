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
# Robust inference by generalized GLS estimator
#  - The random sampling assumption seems to be reasonable within regions, but one might suspect observations from the same region
#    to share some common characteristics, and such characteristics to be possible related to the regressors:
#    Therefore it is advisable to include 5 regional fixed effect, to control for region-related, correlated heterogeneity along the lines of Wooldridge (1020, p.328)
# ------------------------------------------------------------------------------


RiceFarms <- transform(RiceFarms, phosphate = phosphate / 1000, pesticide = as.numeric(pesticide > 0))



# ----------
# We include time effects to control for contemporaneous correlation in the cross section
# We simply introduce 5 separate time effects

fm <- log(goutput) ~ log(seed) + log(urea) + phosphate + log(totlabor) + log(size) + pesticide + varieties + region + time


# OLS
modrice <- plm(fm, RiceFarms, model = "pooling", index = "id")


# Generalised GLS
modrice_ggls <- pggls(fm, RiceFarms, model = "pooling", index = "id")




# ----------
summary(modrice)


summary(modrice_ggls)



# ----------
# convert regression output  --> this produces error
# library(texreg)
# screenreg(list("ols-pool" = modrice, "ggls-pool" = modrice_ggls), digits = 3)
# stargazer::stargazer(modrice, modrice_ggls)



# -->
# Resions do not seem to be so important after all, only Ciwangi being significantly different from the baseline



# ------------------------------------------------------------------------------
# Joint restriction test
# ------------------------------------------------------------------------------

lmtest::waldtest(modrice_ggls, "region")


# -->
# Still joint restriction test rejects, the region effects



# ------------------------------------------------------------------------------
# Excluding region
# ------------------------------------------------------------------------------

modrice_ggls_fe <- pggls(update(fm, .~. - region), RiceFarms, index = "id")


summary(modrice_ggls_fe)



# -->
# Qualitatively, the results do not seem to change much when adding individual fixed effects.



# ------------------------------------------------------------------------------
# Housman test for correlated effects
# ------------------------------------------------------------------------------

# The hypothesis that after controlling for the region, all remaining individual heterogeneity of the random effects type can be tested
# by formally by means of a Hausman test
phtest(modrice_ggls, modrice_ggls_fe)



# -->
# The test does in fact NOT rejected.
# Given the low significance of the regional effects, one might wonder whether a full "random effects" specification can be justified.



# ----------
modrice_ggls_2 <- pggls(update(fm, .~. - region), model = "pooling", RiceFarms, index = "id")


phtest(modrice_ggls_2, modrice_ggls_fe)


# -->
# in fact, omitting the regional fixed effects, Hausman test is passed.
# The 171 rice farms can actually be seen as random draws from the same population, without the need for either individual or regional fixed effects.
