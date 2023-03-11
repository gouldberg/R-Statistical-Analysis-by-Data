setwd("//media//kswada//MyFiles//R//fmri1")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  fMRI Imaging
# ------------------------------------------------------------------------------

data(fmri1, package = "astsa")

str(fmri1)

head(fmri1)


data(fmri, package = "astsa")

str(fmri)

head(fmri)



# ------------------------------------------------------------------------------
# Single Factor Analysis
# ------------------------------------------------------------------------------

bhat = sqrt(lam[1]) * evec[,1]

Dhat = Re(diag(fxx[,,4] - bhat %*% Conj(t(bhat))))



# ----------
# The magnitudes of the elements of the residual matrix at omega = 4/128 are
res = Mod(fxx[,,4] - Dhat - bhat %*% Conj(t(bhat)))

res


# -->
# indicating the residuals are negligible in magnitude and the model fit is good.

