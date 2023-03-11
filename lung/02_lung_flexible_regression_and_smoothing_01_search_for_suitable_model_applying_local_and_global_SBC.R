setwd("//media//kswada//MyFiles//R//lung")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  lung
# ------------------------------------------------------------------------------

lung <- read.csv("data.csv", header = T)

str(lung)

car::some(lung)


# ----------
# only males data

dm <- subset(lung, sex == 1)

dim(dm)



# ----------
# apply a log transformation to height and age

dm <- transform(dm, la = log(age), lh = log(height))



# ------------------------------------------------------------------------------
# Search for a suitable model for fev using BCTo (Box-Cox t original) distribution by stepGAICAll.A
# ------------------------------------------------------------------------------

# Starting from a model m1 with constant parameters
m1 <- gamlss(fev ~ 1, sigma.fo = ~1, nu.fo = ~1, tau.fo = ~1, family = BCTo, data = dm, n.cyc = 100)



# ----------
# Use a local SBC to choose the effective degrees of freedom for smoothing terms
# The reason for using SBC is to achieve smooth centiles.
# A lower value of k (e.g. k = 4) would result in less smooth centiles but a better fit to the data,
# while a higher value of k would result in even smoother centiles, but a worse fit to the data.

k1 <- log(nrow(dm))

m2 <- stepGAICAll.A(m1, scope = list(lower = ~1, upper = ~pb(lh, medhot = "GAIC", k = k1) + pb(la, method = "GAIC", k = k1)), k = k1)



# ----------
summary(m2)



# ------------------------------------------------------------------------------
# Refit the chosen model, but replacing lh and la by log(height) and log(age)
# in order to use predictALl afterwards
# ------------------------------------------------------------------------------

m3 <- gamlss(fev ~ pb((log(height)), method = "GAIC", k = k1) + pb((log(age)), method = "GAIC", k = k1),
             sigma.fo = ~pb((log(height)), method = "GAIC", k = k1) + pb((log(age)), method = "GAIC", k = k1),
             nu.for = ~1, tau.fo = ~1, family = BCTo, data = dm, n.cyc = 100)


summary(m3)



# ------------------------------------------------------------------------------
# Amend mode m3 to fit distribution BCCGo and BCPEo
# ------------------------------------------------------------------------------

m4 <- gamlss(fev ~ pb((log(height)), method = "GAIC", k = k1) + pb((log(age)), method = "GAIC", k = k1),
             sigma.fo = ~pb((log(height)), method = "GAIC", k = k1) + pb((log(age)), method = "GAIC", k = k1),
             nu.for = ~1, tau.fo = ~1, family = BCCGo, data = dm, n.cyc = 100)



m5 <- gamlss(fev ~ pb((log(height)), method = "GAIC", k = k1) + pb((log(age)), method = "GAIC", k = k1),
             sigma.fo = ~pb((log(height)), method = "GAIC", k = k1) + pb((log(age)), method = "GAIC", k = k1),
             nu.for = ~1, tau.fo = ~1, family = BCPEo, data = dm, n.cyc = 100)


GAIC(m3, m4, m5, k = log(nrow(dm)))

             

# -->
# m3 has the lowest SBC



# ------------------------------------------------------------------------------
# Check the adequacy of model m3 using residual diagnostics
# ------------------------------------------------------------------------------

plot(m3)



# ----------
wp(m3, ylim.all = 0.6)

wp(m3, xvar = ~age, n.inter = 9, ylim.worm = 0.8)

wp(m3, xvar = ~height, n.inter = 9, ylim.worm = 0.8)

wp(m3, xvar = ~age + height, n.inter = 4, ylim.worm = 1)



# ----------
Q.stats(m3, xvar = dm$height, n.inter = 25)



# ------------------------------------------------------------------------------
# Effective degrees of freedom (including 2 for the constant and linear terms)
# ------------------------------------------------------------------------------

edfAll(m3)



# ------------------------------------------------------------------------------
# Fitted smooth functions
# ------------------------------------------------------------------------------

term.plot(m3, what = "mu", pages = 1, ask = FALSE)

term.plot(m3, what = "sigma", pages = 1, ask = FALSE)






