# setwd("//media//kswada//MyFiles//R//nondurables")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\nondurables")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  nondurables
#   - the nondurable goods manufacturing index for the United States.
# ------------------------------------------------------------------------------

# data("nondurables", package = "fda")

nondurables <- read.csv("nondurables.txt", header = TRUE) %>% pull()

nondurables <- ts(nondurables, start = 1919, end = 2000, frequency = 12)


str(nondurables)


# this is time-series data




# ------------------------------------------------------------------------------
# Set linear differential operator and smooth
# ------------------------------------------------------------------------------

goodsbasis  = create.bspline.basis(rangeval=c(1919,2000), nbasis=979, norder=8)



# ----------
# Convert integer to linear differential operator
# m = 1:  penalize the square of the slope or valocity
# m = 2:  penalize the squared acceleration
# m = 3:  penalize the squared rate of change of acceleration
# m = 4:  penalize the squared curvature of acceleration
LfdobjNonDur1 = int2Lfd(m = 1)

LfdobjNonDur2 = int2Lfd(m = 2)

LfdobjNonDur3 = int2Lfd(m = 3)

LfdobjNonDur4 = int2Lfd(m = 4)



# ----------
logNondurSm1 = smooth.basisPar(argvals=zoo::index(nondurables),
                              y=log10(zoo::coredata(nondurables)), fdobj=goodsbasis,
                              Lfdobj=LfdobjNonDur1, lambda=1e-11)

logNondurSm2 = smooth.basisPar(argvals=zoo::index(nondurables),
                               y=log10(zoo::coredata(nondurables)), fdobj=goodsbasis,
                               Lfdobj=LfdobjNonDur2, lambda=1e-11)

logNondurSm3 = smooth.basisPar(argvals=zoo::index(nondurables),
                               y=log10(zoo::coredata(nondurables)), fdobj=goodsbasis,
                               Lfdobj=LfdobjNonDur3, lambda=1e-11)

logNondurSm4 = smooth.basisPar(argvals=zoo::index(nondurables),
                               y=log10(zoo::coredata(nondurables)), fdobj=goodsbasis,
                               Lfdobj=LfdobjNonDur4, lambda=1e-11)


logNondurSm4$fd




# ------------------------------------------------------------------------------
# Pairs of Derivatives: Phase-Plane Plots
#   - The second derivative or acceleration curves are plotted against the first derivative or velocity curves
# ------------------------------------------------------------------------------

# A phase-plane plot of the first derivative or velocity and the secnd derivative or acceleration
# of the smoothed log nondurable goods index for 1964
# Midmonths are indicated by the first letters or short abbreviations

graphics.off()

par(mfrow=c(2,2))

phaseplanePlot(1964, logNondurSm1$fd, main = "m = 1")

phaseplanePlot(1964, logNondurSm2$fd, main = "m = 2")

phaseplanePlot(1964, logNondurSm3$fd, main = "m = 3")

phaseplanePlot(1964, logNondurSm4$fd, main = "m = 4")


# phaseplanePlot(1965, logNondurSm$fd)



# ----------
par(mfrow=c(1,1))

phaseplanePlot(1964, logNondurSm4$fd, main = "m = 4")



# -->
# We use the phase-plane plot to study the energy transfer within the economic system.
# There are two large cycles surrounding zero, plus some small cycles that are much closer to the origin.

# The largest cycle begins in mid-May (M), with positive velocity and near zero acceleration.
# Production is increasing linearly or steadily at this point.
# The cycle moves clockwise through June and passes the horizontal zero acceleration line at the end of the month,
# when production is now decreasing linearly.
# By mid-July kinetic energy or velocity is near zero because vacation season is in full swing.
# But potential energy or acceleration is high, and production returns to be positive kinetic/zero potential phase in early August,
# and finally concludes with a cusp at summer's end (S).
# At this point the process looks like it has run out of both potential and kinetic energy.

# The cusp, near where both derivatives are zero, corresponds to the start of school in September and the beginning of the next big production cycle
# passing through the autumn months of October through November.
# Again this large cycle terminates in a small cycle with little potential and kinetic energy.
# This takes up the months of February and March. 
# The tiny subcycle during April and May seems to be due to the spring holidays, since the summer and fall cycles, as well as the cusp,
# do not change much over the next two years, but the spring cycle cusp moves around, 
# reflecting the variability in the timings of Easter and Passover.

