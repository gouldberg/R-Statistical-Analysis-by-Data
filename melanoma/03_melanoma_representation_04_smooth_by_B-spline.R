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
# Smooth by B-spline basis and harmonic acceleration penalty
# ------------------------------------------------------------------------------

# set up the basis with a knot at every year
knots  <- year

nbasis <- nyear + 4

norder <- 6

yearRng = c(1936, 1972)

basisobj  <- create.bspline.basis(yearRng, nbasis, norder, knots)



# smooth the data by penalizing the second derivative
Lfdobj <- 2

lambda <- 1

melafdPar <- fdPar(basisobj, Lfdobj, lambda)

melafd <- smooth.basis(year, mela, melafdPar)$fd




# ------------------------------------------------------------------------------
# Plot 
# ------------------------------------------------------------------------------

par(mfrow = c(1,2))


# plot the data and the smooth
plotfit.fd(mela, year, melafd)
abline(v = c(1940, 1950, 1960, 1970), lty = 2, col = "gray")


# plot the residuals
plotfit.fd(mela, year, melafd, residual=TRUE)
abline(v = c(1940, 1950, 1960, 1970), lty = 2, col = "gray")




# ------------------------------------------------------------------------------
# Compare plots
# ------------------------------------------------------------------------------


par(mfrow = c(1,2))


plotfit.fd(mela, year, melafd, main = "B-spline + harmonic accl penalty")
abline(v = c(1940, 1950, 1960, 1970), lty = 2, col = "gray")


plot(year, mela, type="p", cex=1, xlab="Year", ylab="Cases per 100,000", main = "trend + sinusoidal")
abline(v = c(1940, 1950, 1960, 1970), lty = 2, col = "gray")
lines(year, melahat2, lty=1)
