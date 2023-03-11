setwd("//media//kswada//MyFiles//R//tse")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  tse
#   - Histogram of the Turkish stock exchange returns data
# ------------------------------------------------------------------------------
data("tse", package = "gamlss.data")


str(tse)

car::some(tse)



# ------------------------------------------------------------------------------
# Manually fit alternative distributions
# ------------------------------------------------------------------------------

# two-parameter: normal NO(mu, sigma)
mNO <- histDist(tse$ret, family = "NO", nbins = 30, n.cyc = 100)



# ----------
# three-parameter: t family TF(mu, sigma, nu) and power exponential PE(mu, sigma, nu)
mTF <- histDist(tse$ret, family = "TF", nbins = 30, n.cyc = 100)
mPE <- histDist(tse$ret, family = "PE", nbins = 30, n.cyc = 100)



# ----------
# four-parameter

# Johnson Su
mJSU <- histDist(tse$ret, family = "JSU", nbins = 30, n.cyc = 100)

# skew exponentail power type 1 to 4
mSEP1 <- histDist(tse$ret, family = "SEP1", nbins = 30, n.cyc = 100)
mSEP2 <- histDist(tse$ret, family = "SEP2", nbins = 30, n.cyc = 100)
mSEP3 <- histDist(tse$ret, family = "SEP3", nbins = 30, n.cyc = 100)
mSEP4 <- histDist(tse$ret, family = "SEP4", nbins = 30, n.cyc = 100)

# skew t type 1 to 5
mST1 <- histDist(tse$ret, family = "ST1", nbins = 30, n.cyc = 100)
mST2 <- histDist(tse$ret, family = "ST2", nbins = 30, n.cyc = 100)
mST3 <- histDist(tse$ret, family = "ST3", nbins = 30, n.cyc = 100)
mST4 <- histDist(tse$ret, family = "ST4", nbins = 30, n.cyc = 100)
mST5 <- histDist(tse$ret, family = "ST5", nbins = 30, n.cyc = 100)

# sinh-archsing
mSHASH <- histDist(tse$ret, family = "SHASH", nbins = 30, n.cyc = 100)



# ----------
# compare models
GAIC(m5_2, mNO, mTF, mPE, mSEP1, mSEP2, mSEP3, mSEP4, mST1, mST2, mST3, mST4, mST5, mSHASH, k = 2)
GAIC(m5_2, mNO, mTF, mPE, mSEP1, mSEP2, mSEP3, mSEP4, mST1, mST2, mST3, mST4, mST5, mSHASH, k = 3.84)
GAIC(m5_2, mNO, mTF, mPE, mSEP1, mSEP2, mSEP3, mSEP4, mST1, mST2, mST3, mST4, mST5, mSHASH, k = log(nrow(tse)))




# ----------
# Extract parameters of the best model "SEP2"
mSEP2 <- gamlss(ret ~ 1, data = tse, family = "SEP2")

summary(mSEP2)



# ------------------------------------------------------------------------------
# Check of best model fit
# ------------------------------------------------------------------------------

plot(mSEP2)

wp(mSEP2)
