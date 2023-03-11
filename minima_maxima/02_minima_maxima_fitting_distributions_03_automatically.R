setwd("//media//kswada//MyFiles//R//minima_maxima")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  minima and maxima
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Fit a set of predetermined distributions to the data and choose the best
#   - fitDist() uses gamlssML() to fit a set of predetermined distributions to the data
#     and choose the best according to the GAIC, with default penalty k = 2
# ------------------------------------------------------------------------------

# All of the gamlss.family distributions on the real line have been considered
# realline: continuous distributions on (-inf < y < inf)

f1 <- fitDist(Ymin, type = "realline", k = 2, trace = TRUE)


f2 <- fitDist(Ymax, type = "realline", k = 2, trace = TRUE)


f3 <- fitDist(Ymid, type = "realline", k = 2, trace = TRUE)



# ----------
# Surprisingly !!! ... GU and RG does not good well...

# best model is SEP4
f1$fits


# best model is SN1
f2$fits


# best model is NO
f3$fits



# ------------------------------------------------------------------------------
# Residuals diagnostics
# ------------------------------------------------------------------------------

wp(f1, ylim.all = 0.5)


wp(f2, ylim.all = 0.5)


wp(f3, ylim.all = 0.5)



# ------------------------------------------------------------------------------
# Plot best model fitting
# ------------------------------------------------------------------------------

par(mfcol = c(2,3))

histDist(Ymin, family = GU)
histDist(Ymin, family = SEP4)

histDist(Ymax, family = RG)
histDist(Ymax, family = SN1)

histDist(Ymid, family = NO)
