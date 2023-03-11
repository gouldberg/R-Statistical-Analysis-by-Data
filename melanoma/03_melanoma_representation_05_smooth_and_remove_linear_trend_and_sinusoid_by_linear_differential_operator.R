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

# smooth defines a fourth order linear differential operator that penalizes departure from a linear trend plus sinusoidal trend
# defined by a period of 9.67 years.
# This more sophisticated penalty highlights departures from this type of baseline trend.  

omega   <- 0.65

# Linear Differential Operator Object from a Vector
Lfdobj  <- vec2Lfd(c(0, 0, omega^2, 0), yearRng)


lambda  <- 1e-2

melafdPar2 <- fdPar(basisobj, Lfdobj, lambda)



# ----------
# smooth the data
melafd2 <- smooth.basis(year, mela, melafdPar2)$fd

# plot the results
plotfit.fd(mela, year, melafd2)
abline(v = c(1940, 1950, 1960, 1970), lty = 2, col = "gray")




# ------------------------------------------------------------------------------
# Compare plots
# ------------------------------------------------------------------------------

graphics.off()

par(mfrow = c(1,3))


plotfit.fd(mela, year, melafd2, main = "4th order penalty")
abline(v = c(1940, 1950, 1960, 1970), lty = 2, col = "gray")

plotfit.fd(mela, year, melafd, main = "B-spline + harmonic accl penalty")
abline(v = c(1940, 1950, 1960, 1970), lty = 2, col = "gray")

plot(year, mela, type="p", cex=1, xlab="Year", ylab="Cases per 100,000", main = "trend + sinusoidal")
abline(v = c(1940, 1950, 1960, 1970), lty = 2, col = "gray")
lines(year, melahat2, lty=1)



# -->
# 4th order penalty reveals sub-cycles


