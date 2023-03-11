setwd("//media//kswada//MyFiles//R//hodges")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  hodges
# ------------------------------------------------------------------------------

data("hodges", package = "gamlss.data")


str(hodges)

car::some(hodges)



# ------------------------------------------------------------------------------
# random intercept model by gamlssNP() with various distributions
# ------------------------------------------------------------------------------

library(gamlss.mx)


mNO <- gamlssNP(prind ~ 1, K = 20, random = ~1 | state, mixture = "gq", data = hodges, family = NO)

mLOGNO <- gamlssNP(prind ~ 1, K = 20, random = ~1 | state, mixture = "gq", data = hodges, family = LOGNO)

mBCT <- gamlssNP(prind ~ 1, K = 20, random = ~1 | state, mixture = "gq", data = hodges, family = BCT)



# ----------
mgq20

mNO


# -->
# identical


# ----------
mNO

mLOGNO

mBCT



# ----------
GAIC(mNO, mLOGNO, mBCT, k = log(length(hodges)))



# ------------------------------------------------------------------------------
# Compare sigma_b
# ------------------------------------------------------------------------------

getSigmab(l1)

getSigmab(l2)

mNO$mu.coefficients[2]

mLOGNO$mu.coefficients[2]

mBCT$mu.coefficients[2]



# ------------------------------------------------------------------------------
# Compare residuals
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow=c(2,2), mar = c(2,2,2,2))

plot(resid(mNO))

plot(resid(mLOGNO))

plot(resid(mBCT))



# ------------------------------------------------------------------------------
# Compare fitted values
# ------------------------------------------------------------------------------


graphics.off()
par(mfrow=c(1,1))

plot(prind ~ unclass(state), data = hodges, pch = ".", cex = 2, col = gray(0.6))

points(mNO$ebp ~ unclass(state), data = hodges, pch = 5, col = "red")

points(mLOGNO$ebp ~ unclass(state), data = hodges, pch = 5, col = "red")

points(mBCT$ebp ~ unclass(state), data = hodges, pch = 5, col = "green")

lines(fitted(m0), lty = 2, col = "gray")



# -->
# BCT shrinks more

