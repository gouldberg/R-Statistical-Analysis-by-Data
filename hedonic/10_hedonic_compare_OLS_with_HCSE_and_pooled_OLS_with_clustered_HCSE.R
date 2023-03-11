setwd("//media//kswada//MyFiles//R//hedonic")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Hedonic
# ------------------------------------------------------------------------------

data("Hedonic", package = "plm")


str(Hedonic)


dim(Hedonic)



# ------------------------------------------------------------------------------
# OLS with heteroskedasticity-consistent standard errors
# ------------------------------------------------------------------------------

library(lmtest):  library(plm);


hfm <- mv ~ crim + zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + blacks + lstat



# ----------
# OLS with HC SE

hlmmod <- lm(hfm, data = Hedonic)


coeftest(hlmmod, vcov = vcovHC)




# ------------------------------------------------------------------------------
# pooled OLS with heteroskedasticity-consistent (White) clustered standard errors
# ------------------------------------------------------------------------------

hplmmod <- plm(hfm, data = Hedonic, model = "pooling", index = "townid")


coeftest(hplmmod, vcov = vcovHC)




# ------------------------------------------------------------------------------
# Compare p-values
# ------------------------------------------------------------------------------

sign.tab <- cbind(coef(hlmmod), coeftest(hlmmod, vcov = vcovHC)[,4], coeftest(hplmmod, vcov = vcovHC)[,4])


dimnames(sign.tab)[[2]] <- c("Coefficient", "p-values, HC", "p-val., cluster")


round(sign.tab, 3)



# -->
# proximity to the Charles River, average number of rooms, and the proportion of blacks in the population are not significant any more after clustering town.



