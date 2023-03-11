setwd("//media//kswada//MyFiles//R//stylo")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  stylo
#   - Variables
#        - word:  number of times a word appears in a single text
#        - freq:  frequency of the number of times a word appears in a text
# ------------------------------------------------------------------------------
data("stylo", package = "gamlss.data")


str(stylo)

car::some(stylo)



# ------------------------------------------------------------------------------
# Extend GAMLSS family for truncated at zero count data
# ------------------------------------------------------------------------------

library(gamlss.tr)


# ----------
# Create and fit different truncated at zero count data distributions (PO)
gen.trun(par = 0, family = PO, type = "left")

mPOtr <- gamlss(word ~ 1, weights = freq, data = stylo, family = POtr, trace = TRUE)



# ----------
# NBII
gen.trun(par = 0, family = NBII, type = "left")

mNBIItr <- gamlss(word ~ 1, weights = freq, data = stylo, family = NBIItr, trace = TRUE)



# ----------
# DEL
gen.trun(par = 0, family = DEL, type = "left")

mDELtr <- gamlss(word ~ 1, weights = freq, data = stylo, family = DELtr, trace = TRUE)



# ----------
# SICHEL
gen.trun(par = 0, family = SICHEL, type = "left")

mSICHELtr <- gamlss(word ~ 1, weights = freq, data = stylo, family = SICHELtr, trace = TRUE)



# ----------
GAIC(mPOtr, mNBIItr, mDELtr, mSICHELtr, k = 2)

GAIC(mPOtr, mNBIItr, mDELtr, mSICHELtr, k = 3.84)

GAIC(mPOtr, mNBIItr, mDELtr, mSICHELtr, k = log(nrow(stylo)))



# ----------
# Check residuals of the chosen model

plot(mSICHELtr)

wp(mSICHELtr)



# ----------
# Plot the fitted distributions
histDist(stylo$word, family = SICHELtr, nbins = 30, n.cyc = 100, line.wd = 2.5)







# All of the gamlss.family distributions on the real line have been considered
# realline: continuous distributions on (-inf < y < inf)
m5 <- fitDist(ret, data = tse, type = "realline", k = 2)



# ----------
m5


# ----------
# shows GAIC from best model to worst model
m5$fits


# -->
# The most prefered distribution is the skew exponential power type 2 (SEP2), and
# the least prefered is the reverse Gumbel (RG)


# ----------
# No fails
m5$fails




# ----------
# Repeat with k = 3.84 and k = log(length(tse$ret)) corresponding to criteria X^2(1, 0.05) and Schwartz Bayesian Criteria (SBC), respectively
m6 <- fitDist(ret, data = tse, type = "realline", k = 3.84)

m7 <- fitDist(ret, data = tse, type = "realline", k = log(length(tse$ret)))

m6$fits
m6$fails

m7$fits
m7$fails



# ----------
# plot the best SEP2 model
histDist(ret, data = tse, family = "SEP2", nbins = 30, line.wd = 2.5)



# ----------
# refit the model using gamlss() in order to output the parameter estimates using summary() 
m5_2 <- gamlss(ret ~ 1, data = tse, family = "SEP2")

summary(m5_2)



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

# sinh arc-sing
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
