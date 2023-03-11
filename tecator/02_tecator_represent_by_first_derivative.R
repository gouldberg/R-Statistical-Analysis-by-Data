setwd("//media//kswada//MyFiles//R//tecator")

# packages <- c("dplyr", "AppliedPredictiveModeling", "caret", "lattice", "fda.usc")
packages <- c("dplyr", "caret", "lattice", "fda.usc")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  tecator
# ------------------------------------------------------------------------------
# data("tecator", package = "caret")
data("tecator", package = "fda.usc")



# ------------------------------------------------------------------------------
# Represent absorbance curves and their derivatives
# ------------------------------------------------------------------------------

absorp <- tecator$absorp.fdata

( Fat20 <- ifelse(tecator$y$Fat < 20, 0, 1) * 2 + 2 )


str(absorp)

# this is fdata class  --> can use fdata.deriv()
class(absorp)



# ----------
absorp.dl <- fdata.deriv(absorp, nderiv = 1)

graphics.off()
par(mfrow=c(1,2))
plot(absorp, col = Fat20, ylab = " ", xlab = "Wavelength", main = "Absorbances")
plot(absorp.dl, col = Fat20, ylab = " ", xlab = "Wavelength", main = "Derivatives")

