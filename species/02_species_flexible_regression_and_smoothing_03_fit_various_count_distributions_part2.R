setwd("//media//kswada//MyFiles//R//species")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  species
# ------------------------------------------------------------------------------
data("species", package = "gamlss.data")


str(species)

car::some(species)



# ----------
species <- transform(species, x = log(lake))



# ------------------------------------------------------------------------------
# Fit various count distributions  Part II
# ------------------------------------------------------------------------------

library(gamlss.mx)
library(gamlss.cens)


m1 <- gamlss(fish ~ poly(x, 2), data = species, family = PO, trace = FALSE)
m2 <- gamlss(fish ~ x, data = species, family = NBI, trace = FALSE)
m3 <- gamlss(fish ~ poly(x, 2), data = species, family = NBI, trace = FALSE)
m4 <- gamlss(fish ~ cs(x, 2), data = species, family = NBI, trace = FALSE)
m5 <- gamlss(fish ~ poly(x, 2), sigma.fo = ~x, data = species, family = NBI, trace = FALSE)
m6 <- gamlss(fish ~ poly(x, 2), sigma.fo = ~1, data = species, family = NBF, n.cyc = 200, trace = FALSE)
m7 <- gamlss(fish ~ poly(x, 2), sigma.fo = ~x, data = species, family = NBF, n.cyc = 100, trace = FALSE)

m8 <- gamlss(fish ~ poly(x, 2), data = species, family = PIG, trace = FALSE)
m9 <- gamlss(fish ~ poly(x, 2), nu.fo = ~x, data = species, family = SICHEL, trace = FALSE)
m10 <- gamlss(fish ~ poly(x, 2), nu.fo = ~x, data = species, family = DEL, n.cyc = 50, trace = FALSE)
m11 <- gamlss(fish ~ poly(x, 2), nu.fo = ~x, data = species, family = DEL, sigma.fix = TRUE, sigma.start = 1, n.cyc = 50, trace = FALSE)


# normal random effect mixture distributions
m12 <- gamlssNP(fish ~ poly(x, 2), data = species, mixture = "gq", K = 20, family = PO, control = NP.control(trace = FALSE))
m13 <- gamlssNP(fish ~ poly(x, 2), sigma.fo = ~x, data = species, mixture = "gq", K = 20, family = NBI, control = NP.control(trace = FALSE))


# non parametric random effects distributions
m14 <- gamlssNP(fish ~ poly(x, 2), data = species, mixture = "np", K = 6, tol = 0.1, family = PO, control = NP.control(trace = FALSE))
m15 <- gamlssNP(fish ~ poly(x, 2), data = species, mixture = "np", K = 2, family = NBI, control = NP.control(trace = FALSE))


# Effron's double exponential (Poisson) distribution
m16 <- gamlss(fish ~ poly(x, 2), nu.fo = ~x, data = species, family = DPO, trace = FALSE)


# Discrete inverse Gaussian distribution
m17 <- gamlss(Surv(fish, fish + 1, type = "interval2") ~ x + I(x^2), sigma.fo = ~1, data = species, family = cens(IG, type = "interval"), trace = FALSE)



# ----------
GAIC(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17)

GAIC(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17, k = log(70))



# -->
# The best fitted models are m9 and m17 as suggested by AIC and SBC, respectively.

# Comparing models 2, 3, and 4:
# quadratic model for log mu is found to be adequate.

# Comparing models 1 and 3:
# Y ahs a highly overdispersed Poisson distribution.

# Comparing models 3 with 5 and 6:
# either a linear model in x for log(sigma) or a different variance-mean relationship from that of the negative binomial [i.e. V{Y} = mu + sigma * mu^2] is required.
# In particular the estimated nu parameter in the negative binomial family (NBF) of model 6 is nu = 2.9 suggesting a possible
# variance-mean relationship V[Y} = mu + sigma * mu^3]

# model 7:
# Modeling sigma in the NBF did not improve the fit greatly.



# ------------------------------------------------------------------------------
# Diagnostics: worm plots
# ------------------------------------------------------------------------------

wp(m9);  title("(a)")

wp(m17);  title("(b)")



# ------------------------------------------------------------------------------
# Refit model 9 to extract model coefficient
# ------------------------------------------------------------------------------

# For model 9, refit the model using an ordinary quadratic polynomial in x for log(mu), rather than the orthogonal quadratic polynomial
mSI <- gamlss(fish ~ x + I(x ^ 2), sigam.fo = ~1, nu.fo = ~x, data = species, family = SICHEL, trace = FALSE)

GAIC(m9, mSI)



# ----------
summary(mSI)





