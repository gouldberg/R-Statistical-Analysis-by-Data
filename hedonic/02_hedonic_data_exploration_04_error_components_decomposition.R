setwd("//media//kswada//MyFiles//R//hedonic")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Hedonic
# ------------------------------------------------------------------------------

data("Hedonic", package = "plm")


str(Hedonic)


dim(Hedonic)


car::some(Hedonic)



# ----------
Hp <- pdata.frame(Hedonic, index = "townid")



# ------------------------------------------------------------------------------
# Error components decomposition
# (by GLS estimator to remove individual means)
# ------------------------------------------------------------------------------

hfm <- mv ~ crim + zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + blacks + lstat

ercomp(hfm, Hp)



# -->
# Here for the error, the share of the individual effect and that of the idiosyncratic effect are 43.8% and 56.2%, respectively

# theta = 0.6284 at median:  GLS estimator removes about only 62.8% of the individual mean

