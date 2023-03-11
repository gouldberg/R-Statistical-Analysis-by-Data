setwd("//media//kswada//MyFiles//R//swedish_mortality")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Swedish Mortality
# ------------------------------------------------------------------------------



# ------------------------------------------------------------------------------
# Smoothing Log Hazard Rate for each of 144 birth year cohorts
# ------------------------------------------------------------------------------

nbasis = 85

norder = 6

SwedeTime = 0:80

SwedeBasis = create.bspline.basis(SwedeTime, nbasis, norder)

D2fdPar = fdPar(SwedeBasis, lambda=1e-7)

SwedeLogHazfd = smooth.basis(SwedeTime, SwedeLogHazard, D2fdPar)$fd


# ----------
plot(SwedeLogHazfd)



# ----------
# plot for each of 144 birth year cohorts

plotfit.fd(SwedeLogHazard, SwedeTime, SwedeLogHazfd)
