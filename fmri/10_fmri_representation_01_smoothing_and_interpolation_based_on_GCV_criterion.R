setwd("//media//kswada//MyFiles//R//fmri1")

packages <- c("dplyr", "fda")
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
# Now we focus on average across subjects of "Awake-Heat" and "Awake-Shock"
# Averaging "Awake-Heat" and "Awake-Shock"
# ------------------------------------------------------------------------------

# Awake-Heat
ah <- list(fmri$L1T2, fmri$L2T2, fmri$L3T2, fmri$L4T2, fmri$L5T2, fmri$L6T2, fmri$L7T2, fmri$L8T2, fmri$L9T2)

ah <- bind_cols(lapply(1:length(ah), function(x) rowMeans(as.matrix(ah[[x]])))) %>% as.data.frame()

colnames(ah) <- c("L1T2", "L2T2", "L3T2", "L4T2", "L5T2", "L6T2", "L7T2", "L8T2", "L9T2")


# Awake-Shock
as <- list(fmri$L1T3, fmri$L2T3, fmri$L3T3, fmri$L4T3, fmri$L5T3, fmri$L6T3, fmri$L7T3, fmri$L8T3, fmri$L9T3)

as <- bind_cols(lapply(1:length(as), function(x) rowMeans(as.matrix(as[[x]])))) %>% as.data.frame()

colnames(as) <- c("L1T3", "L2T3", "L3T3", "L4T3", "L5T3", "L6T3", "L7T3", "L8T3", "L9T3")


plot.ts(ah, main = "Awake-Heat average across subjects")

plot.ts(as, main = "Awake-Shock average across subjects")



# ------------------------------------------------------------------------------
# Smoothing and interpolation by fourier basis based on GCV criterion
# Choose roughness penalty lambda by GCV criterion  
# ------------------------------------------------------------------------------
# Set up the argument values: equally spaced over circle of circumference 20.
# Earlier analyses of the gait data used time values over [0,1],
# but led to singularity problems in the use of function fRegress.
# In general, it is better use a time interval that assigns roughly one time unit to each inter-knot interval.

astime = seq(1, 128, 1)

asrange = c(-1, 1)

asfine = seq(0, 128, len = 128*10)



# ----------
# The harmonic acceleration roughness penalty is used throughout since the data are periodic with a strong sinusoidal component of variation.
harmaccelLfd20 = vec2Lfd(c(0, (2 * pi / 20)^2, 0), rangeval=asrange)

asbasis = create.fourier.basis(asrange, nbasis = 21)



# ----------
# First select smoothing for the raw data
asLoglam = seq(-4, 0, 0.25)

nglam   = length(asLoglam)

asSmoothStats = array(NA, dim=c(nglam, 3), dimnames=list(asLoglam, c("log10.lambda", "df", "gcv") ) )

asSmoothStats[, 1] = asLoglam

asSmoothStats



# ----------
# loop through smoothing parameters

for (ilam in 1:nglam) {
  asSmooth = smooth.basisPar(astime, as, asbasis,
                               Lfdobj=harmaccelLfd20, lambda=10^asLoglam[ilam])
  asSmoothStats[ilam, "df"]  = asSmooth$df
  asSmoothStats[ilam, "gcv"] = sum(asSmooth$gcv)
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



# ----------
# Plot individual curves and its mean of knee angle and hip angle
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




