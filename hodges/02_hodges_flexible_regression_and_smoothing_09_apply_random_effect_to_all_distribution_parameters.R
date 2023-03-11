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
# Fit random effect for state, addtionally models for sigma, bu, and tau.
# ------------------------------------------------------------------------------

library(gamlss.mx)


# We consider a Box-Cox t (BCT) distribution model, which allows for both skewness and kurtosis in the conditional distribution of Y
# and allows for differences between states, in the location, scale and shape of the conditional distribution of Y,
# by including a random effect term in each of the models for the parameters mu, sigma, nu and tau.

m0 <- gamlss(prind ~ random(state), data = hodges, family = BCT, trace = FALSE)


m1 <- gamlss(prind ~ random(state), sigma.fo = ~random(state), data = hodges, family = BCT, trace = FALSE)


m2 <- gamlss(prind ~ random(state), sigma.fo = ~random(state), nu.fo = ~random(state), data = hodges, family = BCT, trace = FALSE)


m3 <- gamlss(prind ~ random(state), sigma.fo = ~random(state), nu.fo = ~random(state), tau.fo = ~random(state), data = hodges, family = BCT, trace = FALSE)



# ----------
AIC(mNO, mLOGNO, mBCT, m0, m1, m2, m3)



# -->
# AIC indicates that the random effects for nu and tau are not needed.
# Model m1 has the lowest AIC, although it is only very slightly better than m0.
# Indeed a slightly higher penalty k than the default 2 in GAIC would result in the selection of model m0.

# In practive one would probably prefer m0 on the grounds of parsimony.



# ----------
m0


# fitted degrees of freedom and the random effect parameter sigma1 (sigma_b) for mu
getSmo(m0)



# ------------------------------------------------------------------------------
# plot observed and fitted medians of prind against state
# ------------------------------------------------------------------------------

# total number of observations per state
( total <- with(hodges, tapply(prind, state, "length")) )


# median per state
( ho.median <- with(hodges, tapply(prind, state, "median")) )


# getting fitted medians
( f.median <- qBCT(0.50, mu = fitted(m0), sigma = fitted(m0, "sigma"), nu = fitted(m0, "nu"), tau = fitted(m0, "tau")) )


# fitted median by state
( fitted.m <- tapply(f.median, hodges$state, median) )




# quantities needed for plotting
label <- attr(ho.median, "dimnames")

lll <- label[[1]]

index <- 1:45



# ----------
graphics.off()
plot(ho.median[order(ho.median)], ylab = "Median monthly premium ($)",
     xlab = "State (sorted by sample median monthly premium)",
     ylim = c(90, 260), xaxt = "n", cex.lab = 1.5, cex.axis = 1, cex.main = 1.2)


points(fitted.m[order(ho.median)], pch = 3, col = "blue", cex = 0.7)

text(index, rep(95, 45), lll[order(ho.median)], cex = 0.65)
text(index, rep(100, 45), total[order(ho.median)], cex = 0.65)

legend("topleft", legend = c("observed", "fitted"), pch = c(1,3), col = c("black", "blue"))




# ------------------------------------------------------------------------------
# Compare fitted values
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow=c(1,1))

plot(prind ~ unclass(state), data = hodges, pch = ".", cex = 2, col = gray(0.6))

points(mBCT$ebp ~ unclass(state), data = hodges, pch = 5, col = "red")

points(fitted(m0) ~ unclass(state), data = hodges, pch = 5, col = "green")




