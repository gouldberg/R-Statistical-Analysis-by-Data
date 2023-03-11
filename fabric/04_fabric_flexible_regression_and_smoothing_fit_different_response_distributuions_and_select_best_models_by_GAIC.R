setwd("//media//kswada//MyFiles//R//abdom")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  fabric
# ------------------------------------------------------------------------------
data("fabric", package = "gamlss.data")


str(fabric)

car::some(fabric)



# ------------------------------------------------------------------------------
# Fit two-patameter distributions
# ------------------------------------------------------------------------------
# Negative Binomial Type 1
mNBI <- gamlss(y ~ pb(x), sigma.fo = ~pb(x), data = fabric, family = NBI)


# Negative Binomial Type 2
mNBII <- gamlss(y ~ pb(x), sigma.fo = ~pb(x), data = fabric, family = NBII)


# Poisson inverse Gaussian
mPIG <- gamlss(y ~ pb(x), sigma.fo = ~pb(x), data = fabric, family = PIG)


# Zero inflated Poisson
mZIP <- gamlss(y ~ pb(x), sigma.fo = ~pb(x), data = fabric, family = ZIP)


# Zero altered Poisson
mZAP <- gamlss(y ~ pb(x), sigma.fo = ~pb(x), data = fabric, family = ZAP)



# ------------------------------------------------------------------------------
# Fit three-patameter distributions
# ------------------------------------------------------------------------------

# Sichel (mu the mean)
mSICHEL <- gamlss(y ~ pb(x), sigma.fo = ~pb(x), data = fabric, family = SICHEL)


# zero inflatged neg.binomial
mZINBI <- gamlss(y ~ pb(x), sigma.fo = ~pb(x), nu.fo = ~ pb(x), data = fabric, family = ZINBI)


# Zero altered neg.binomial
mZANBI <- gamlss(y ~ pb(x), sigma.fo = ~pb(x), nu.fo = ~ pb(x), data = fabric, family = ZANBI)



# ------------------------------------------------------------------------------
# Compare models by GAIC
# ------------------------------------------------------------------------------

GAIC(mPO1, mPO2, mNBI, mNBII, mPIG, mZIP, mZAP, mSICHEL, mZINBI, mZANBI, k = 2)


# -->
# Poisson inverse Gaussian (PIC) is the best for AIC



# ----------
GAIC(mPO1, mPO2, mNBI, mNBII, mPIG, mZIP, mZAP, mSICHEL, mZINBI, mZANBI, k = 3)

GAIC(mPO1, mPO2, mNBI, mNBII, mPIG, mZIP, mZAP, mSICHEL, mZINBI, mZANBI, k = log(length(fabric$y)))



# ------------------------------------------------------------------------------
# Check Poisson-Inverse Gaussian Distribution
# ------------------------------------------------------------------------------
PIG()


# ----------
?gamlss.family



# ----------
graphics.off()
par(mfrow=c(4,4), mar = c(2,2,2,2))
for(i in 1:15) plot(function(y) dPIG(y, mu=4, sigma = i), from=0, to=50, n=50+1, type="h", main = paste0("sigma = ", i))
plot(function(y) dPO(y, mu=4), from=0, to=50, n=50+1, type="h", main = "Poisson")



# ------------------------------------------------------------------------------
# Check the residuals of best model (here mLO)
# ------------------------------------------------------------------------------

plot(mPIG)



# ----------
# worm plot

wp(mPIG)



# ------------------------------------------------------------------------------
# Represent chosen model
# ------------------------------------------------------------------------------

# total effective degrees of freedom
edfAll(mPIG)



# ----------
# plot fitted parameters
fittedPlot(mPIG, x = fabric$x)



# ----------
plot(y ~ x, data = fabric)
# plot(fitted(mPIG) ~ x, data = fabric)
lines(fitted(mPIG)[order(x)] ~ fabric$x[order(x)], data = fabric, col = "blue")



# ----------
# centile curves
centiles(mPIG, fabric$x)

centiles.fan(mPIG, xvar = fabric$x, cent = c(3, 10, 25, 50, 75, 90, 97), colors = "terrain", ylab = "y", xlab = "x")



# ----------
library(gamlss.util)
plotSimpleGamlss(y, x, model = mPIG, data = fabric)

