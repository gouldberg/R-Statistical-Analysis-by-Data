setwd("//media//kswada//MyFiles//R//abdom")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  abdom
# ------------------------------------------------------------------------------

data("abdom", package = "gamlss.data")


str(abdom)

car::some(abdom)



# ------------------------------------------------------------------------------
# Fit the normal distribution model
# ------------------------------------------------------------------------------

# Fit the normal distribution model, using pb() to fit P-spline smoothers for the predictors for mu and sigma with automatic selection of smoothing parameters
mNO <- gamlss(y ~ pb(x), sigma.fo = ~pb(x), data = abdom, family = NO)



# ------------------------------------------------------------------------------
# Fit two-patameter distributions
# ------------------------------------------------------------------------------
# gamma
mGA <- gamlss(y ~ pb(x), sigma.fo = ~pb(x), data = abdom, family = GA)


# inverse gamma
mIG <- gamlss(y ~ pb(x), sigma.fo = ~pb(x), data = abdom, family = IG)


# Gumbel
mGU <- gamlss(y ~ pb(x), sigma.fo = ~pb(x), data = abdom, family = GU)


# reverse Gumbel
mRG <- gamlss(y ~ pb(x), sigma.fo = ~pb(x), data = abdom, family = RG)


# logistic
mLO <- gamlss(y ~ pb(x), sigma.fo = ~pb(x), data = abdom, family = LO)



# ------------------------------------------------------------------------------
# Fit three-patameter distributions
# ------------------------------------------------------------------------------

# power exponential
mPE <- gamlss(y ~ pb(x), sigma.fo = ~pb(x), nu.fo = ~ pb(x), data = abdom, family = PE)


# t Family
mTF <- gamlss(y ~ pb(x), sigma.fo = ~pb(x), nu.fo = ~ pb(x), data = abdom, family = TF)


# Box-Cox Cole Green
mBCCG <- gamlss(y ~ pb(x), sigma.fo = ~pb(x), nu.fo = ~ pb(x), data = abdom, family = BCCG)



# ------------------------------------------------------------------------------
# Fit four-patameter distributions
# ------------------------------------------------------------------------------

# Box-Cox t
mBCT <- gamlss(y ~ pb(x), sigma.fo = ~pb(x), nu.fo = ~ pb(x), data = abdom, family = BCT)


# Box-Cox power exponential
mBCPE <- gamlss(y ~ pb(x), sigma.fo = ~pb(x), nu.fo = ~ pb(x), data = abdom, family = BCPE)



# ------------------------------------------------------------------------------
# Compare models by GAIC
# ------------------------------------------------------------------------------

GAIC(mNO, mGA, mIG, mGU, mRG, mLO, mPE, mTF, mBCCG, mBCT, mBCPE, k = 2)



# ------------------------------------------------------------------------------
# Check the residuals of best model (here mLO)
# ------------------------------------------------------------------------------

plot(mLO)


# ----------
# worm plot

wp(mLO)



# ------------------------------------------------------------------------------
# Represent chosen model
# ------------------------------------------------------------------------------

# total effective degrees of freedom
edfAll(mLO)



# ----------
# plot fitted parameters
fittedPlot(mLO, x = abdom$x)



# ----------
plot(y ~ x, data = abdom)
lines(fitted(mLO) ~ x, data = abdom, col = "blue")



# ----------
# centile curves
centiles(mLO, abdom$x)

centiles.fan(mLO, xvar = abdom$x, cent = c(3, 10, 25, 50, 75, 90, 97), colors = "terrain", ylab = "y", xlab = "x")



# ----------
plotSimpleGamlss(y, x, model = mLO, data = abdom, cols = heat_hcl(100))

