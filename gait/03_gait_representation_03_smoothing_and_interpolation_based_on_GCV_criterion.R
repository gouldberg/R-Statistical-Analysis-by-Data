setwd("//media//kswada//MyFiles//R//gait")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  gait
# ------------------------------------------------------------------------------

data("gait", package = "fda")

str(gait)

dim(gait)



# ------------------------------------------------------------------------------
# Smoothing and interpolation by fourier basis based on GCV criterion
# Choose roughness penalty lambda by GCV criterion  
# ------------------------------------------------------------------------------
# Set up the argument values: equally spaced over circle of circumference 20.
# Earlier analyses of the gait data used time values over [0,1],
# but led to singularity problems in the use of function fRegress.
# In general, it is better use a time interval that assigns roughly one time unit to each inter-knot interval.

gaittime = seq(0.5, 19.5, 1)

gaitrange = c(0, 20)

gaitfine = seq(0, 20, len = 101)



# ----------
# The harmonic acceleration roughness penalty is used throughout since the data are periodic with a strong sinusoidal component of variation.
harmaccelLfd20 = vec2Lfd(c(0, (2 * pi / 20)^2, 0), rangeval=gaitrange)

gaitbasis = create.fourier.basis(gaitrange, nbasis = 21)



# ----------
# First select smoothing for the raw data
gaitLoglam = seq(-4, 0, 0.25)

nglam   = length(gaitLoglam)

gaitSmoothStats = array(NA, dim=c(nglam, 3), dimnames=list(gaitLoglam, c("log10.lambda", "df", "gcv") ) )

gaitSmoothStats[, 1] = gaitLoglam

gaitSmoothStats



# ----------
# loop through smoothing parameters

for (ilam in 1:nglam) {
  gaitSmooth = smooth.basisPar(gaittime, gait, gaitbasis,
                               Lfdobj=harmaccelLfd20, lambda=10^gaitLoglam[ilam])
  gaitSmoothStats[ilam, "df"]  = gaitSmooth$df
  gaitSmoothStats[ilam, "gcv"] = sum(gaitSmooth$gcv)
  # note: gcv is a matrix in this case
}



# -----------
# display and plot GCV criterion and degrees of freedom

gaitSmoothStats

par(mfrow=c(1,1))
plot(gaitSmoothStats[, c("log10.lambda", "gcv")], type='b')



# ----------
par(mfrow=c(2,1))

plot(gaitSmoothStats[, c("log10.lambda", "gcv")], type="b", log="y")

plot(gaitSmoothStats[, c("log10.lambda", "df")], type="b", log="y")



# ------------------------------------------------------------------------------
# Smoothing and interpolation by fourier basis based on GCV criterion
# Smooth and interpolate by best lambda
# ------------------------------------------------------------------------------

# GCV is minimized with lambda = 10^(-1.5).

gaitSmooth = smooth.basisPar(gaittime, gait,
                             gaitbasis, Lfdobj=harmaccelLfd20, lambda=10^(-1.5))
gaitfd = gaitSmooth$fd

names(gaitfd$fdnames) = c("Normalized time", "Child", "Angle")

gaitfd$fdnames[[3]] = c("Hip", "Knee")

gaitfd



# ----------
hipfd  = gaitfd[,1]

kneefd = gaitfd[,2]



# ----------
# take means
kneefdMean = mean(kneefd)

hipfdMean = mean(hipfd)



# ------------------------------------------------------------------------------
# Plot individual curves and its mean of knee angle and hip angle
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow=c(2,1))

plot(kneefd, xlab = "", ylab = "", ylim = c(0,80), main = "Mean Knee Angle")
lines(kneefdMean, lwd=3)

plot(hipfd, xlab = "", ylab = "", ylim = c(0,80), main = "Mean Hip Angle")
lines(hipfdMean, lwd = 3)



# ----------
# Alternatively, mean curves
gaitmeanfd <- mean.fd(gaitfd)

plot(gaitmeanfd)




