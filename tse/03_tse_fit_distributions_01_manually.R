setwd("//media//kswada//MyFiles//R//tse")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  tse
# ------------------------------------------------------------------------------
data("tse", package = "gamlss.data")


str(tse)

car::some(tse)




# ------------------------------------------------------------------------------
# Fit predetermined distribution
# ------------------------------------------------------------------------------

# Fit t family (TF) distribution
# fitting method is "RS"
m1 <- gamlss(ret ~ 1, data = tse, family = TF)




# ----------
# gamlssML() uses numerical optimization techniques that are faster than the algorithms that gamlss() uses, which are designed for regression models.
# fitting method is "nlminb"
m2 <- gamlssML(ret, data = tse, family = TF)



# ----------
# histDist() uses gamlssMS() as a default algorithm to fit the model, but in addition displays the histogram together with the fitted distribution of the data
m3 <- histDist(ret, data = tse, family = TF, nbins = 30, line.wd = 2.5)



# ----------
m1

m2

m3
