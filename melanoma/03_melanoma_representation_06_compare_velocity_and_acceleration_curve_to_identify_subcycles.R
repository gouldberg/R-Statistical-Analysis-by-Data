# setwd("//media//kswada//MyFiles//R//melanoma")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\melanoma")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  melanoma
# ------------------------------------------------------------------------------

melanoma <- t(matrix(scan("melanoma.txt", 0), 3, 37)) %>% as.data.frame() %>% dplyr::select(V2, V3)

# data("melanoma", package = "fda")


colnames(melanoma) <- c("year", "incidence")



str(melanoma)




# ------------------------------------------------------------------------------
# Remove sinusoid plus trend by operator
# ------------------------------------------------------------------------------

year  <- melanoma[,1]

mela  <- melanoma[,2]

nyear <- length(year)

yearRng = c(1936, 1972)



# ----------
# Smooth by Fourier basis
nbasis = 9

lambda = 10 ^ 0.5

yearbasis = create.fourier.basis(yearRng, nbasis)

Sm = smooth.basisPar(argvals = t36.72,
                     y = melanoma$incidence, 
                     fdobj = yearbasis,
                     Lfdobj = Lfdobj, lambda = lambda)



# ----------
# Smooth by B-Spline wih a harmonic acceleration penalty
knots  <- year

nbasis <- nyear + 4

norder <- 6

basisobj  <- create.bspline.basis(yearRng, nbasis, norder, knots)



# smooth the data by penalizing the second derivative
Lfdobj <- 2

lambda <- 1

melafdPar <- fdPar(basisobj, Lfdobj, lambda)

melafd <- smooth.basis(year, mela, melafdPar)$fd




# Smooth and Remove linear trend and sinusoidal by linear differential operator
omega   <- 0.65

# Linear Differential Operator Object from a Vector
Lfdobj  <- vec2Lfd(c(0, 0, omega^2, 0), yearRng)

lambda  <- 1e-2

melafdPar_l <- fdPar(basisobj, Lfdobj, lambda)

melafd_l <- smooth.basis(year, mela, melafdPar_l)$fd




# ------------------------------------------------------------------------------
# compare by velocity and acceleration curve
# ------------------------------------------------------------------------------

graphics.off()
op <- par(mfrow=c(3,3), mar=c(2,2,2,2), pty="m", ask=FALSE)

plot(year, mela, type="p", cex=1, xlab="", ylab = "", main="Fourier basis smoother")
abline(v = c(1940, 1950, 1960, 1970), lty = 2, col = "gray")
lines(Sm)

plot(Sm, Lfd=1, ylab="", main="Velocity", cex=1.2)
abline(v = c(1940, 1950, 1960, 1970), lty = 2, col = "gray")

plot(Sm, Lfd=2, ylab="", main="Acceleration", cex=1.2)
abline(v = c(1940, 1950, 1960, 1970), lty = 2, col = "gray")



# ----------
plot(year, mela, type="p", cex=1, xlab="", ylab = "", main="B-Spline basis smoother")
abline(v = c(1940, 1950, 1960, 1970), lty = 2, col = "gray")
lines(melafd)

plot(melafd, Lfd=1, ylab="", main="Velocity", cex=1.2)
abline(v = c(1940, 1950, 1960, 1970), lty = 2, col = "gray")

plot(melafd, Lfd=2, ylab="", main="Acceleration", cex=1.2)
abline(v = c(1940, 1950, 1960, 1970), lty = 2, col = "gray")



# ----------
plot(year, mela, type="p", cex=1, xlab="", ylab = "", main="Smoothing by Linear Differential Operator")
abline(v = c(1940, 1950, 1960, 1970), lty = 2, col = "gray")
lines(melafd_l)

plot(melafd_l, Lfd=1, ylab="", main="Velocity", cex=1.2)
abline(v = c(1940, 1950, 1960, 1970), lty = 2, col = "gray")

plot(melafd_l, Lfd=2, ylab="", main="Acceleration", cex=1.2)
abline(v = c(1940, 1950, 1960, 1970), lty = 2, col = "gray")



# -->
# Smoothing by B-splines and linear differential operator procide somewhat evidence of existence of subcycles 



# ------------------------------------------------------------------------------
# segment by years
# ------------------------------------------------------------------------------


par(mfrow=c(2,2), mar = c(2,2,2,2))

yr <- c(1936, 1944, 1953, 1961, 1970)


plot(year, mela, type="p", cex=1, xlab="", ylab = "", main="Smoothing by Linear Differential Operator")
lines(melafd_l)
abline(v = yr, lty = 2, col = "gray")

plot(melafd_l, Lfd=1, ylab="", main="Velocity", cex=1.2)
abline(v = yr, lty = 2, col = "gray")

plot(melafd_l, Lfd=2, ylab="", main="Acceleration", cex=1.2)
abline(v = yr, lty = 2, col = "gray")


