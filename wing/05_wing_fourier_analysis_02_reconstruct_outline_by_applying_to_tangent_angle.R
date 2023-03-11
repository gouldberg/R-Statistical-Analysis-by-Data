setwd("//media//kswada//MyFiles//R//wing")

packages <- c("dplyr", "pixmap")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

source(file = "//media//kswada//MyFiles//R_basics//functions_for_morphometrics.R")



# ------------------------------------------------------------------------------
# data:  wing
# ------------------------------------------------------------------------------

M <- read.pnm("//media//kswada//MyFiles//references//MorphometricsWithR//e-supplement//wing.ppm")

M


str(M)


# -----------
plot(M)



# ------------------------------------------------------------------------------
# Smooth the outline to avoid imperfection due to automatic digitization
#   - We have to determine the number i of smoothing iterations.
#     Haines and Crampton recommend having ntot / p < sqrt(i / 2) where ntot is the total number of pixels of the outline
#     and p is the number of points to be equally sampled on the outline
# ------------------------------------------------------------------------------


( numb <- 2 * (length(X) / 64) ^ 2 )

par(mfrow=c(1,1), mar=c(2,2,2,2))

M2 <- smoothout(cbind(X, Y), numb + 1)

M2


plot(M2)



# ------------------------------------------------------------------------------
# Select equally spaced points on the outline
# ------------------------------------------------------------------------------

X64 <- (M2[,1][seq(1, length(X), length = 65)])[-1]

Y64 <- (M2[,2][seq(1, length(X), length = 65)])[-1]



# ------------------------------------------------------------------------------
# Reconstruct of outlines by Fourier analysis applied to tangent angle as a function of the perimeter
#   - The method consists of describing the cumulative change in angle of a tangent vector to the outline as a funciton of the cumulative chordal distance
#     t along the curve.
# ------------------------------------------------------------------------------

# n:  number of harmonics
n <- 32
f2 <- fourier2(cbind(X64, Y64), n)


# n:  number of harmonics
k <- 20
if2 <- ifourier2(f2$ao, f2$an, f2$bn, 64, k, thetao = f2$thetao)



# ----------
graphics.off()

layout(matrix(c(1,2), 1, 2))

plot(if2$X, if2$Y, type = "l", asp = 1, xlab = "X", ylab = "Y", main = paste0("harmonics 0 to ", k))


# The reconstruction of the outliine is not always nice because estimating tangent angles
# can result in aberrant shapes
plot(f2$t, f2$phi, type = "l", lwd = 2, col = "grey70", xlab = "Curvilinear abscissa", ylab = "Tangent angle",
     main = "Variation of tangent angle")

# An alternative option consists of analyzing differences in reconstruction of variation of tangent vector to the outline according to t, the curvilinear abscissa
lines(if2$angle, if2$phi)



# -->
# Left panel: the reconstruction of the outline
# Right panel: the fit of the signal by the Fourier series (black line) on the actual variaion of the tangent angle (thick gray line)

