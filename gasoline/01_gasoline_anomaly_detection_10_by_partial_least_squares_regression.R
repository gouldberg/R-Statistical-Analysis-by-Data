rm(list = ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\gasoline")



# ------------------------------------------------------------------------------
# data:  gasoline
#   - data set consisting of octane number (octane) and NIR spectra (NIR) of 60 gasoline samples
#     Each NIR spectrum consists of 401 diffuse reflectance measurements from 900 to 1700 nm
# ------------------------------------------------------------------------------


data(gasoline, package = "pls")


dim(gasoline)


gasoline$octane



# ----------
# 60 * 401

dim(gasoline$NIR)

str(gasoline$NIR)





# ------------------------------------------------------------------------------
# data exploration
# ------------------------------------------------------------------------------


graphics.off()

par(mfrow = c(2,1))

hist(gasoline$octane, main = "distribution of octane numer for 60 gasoline samples")

matplot(t(gasoline$NIR), type = "l", main = "X: diffuse reflectance measurements (401 type)  Y: NIR spectra for 60 gasoline samples")



# -->
# NIR spectra for for 60 gasoline samples are very close at each measurement



# ------------------------------------------------------------------------------
# data exploration
# ------------------------------------------------------------------------------


# plot Octane vs. NIR measurement


graphics.off()

par(mfrow = c(4,4))


for(i in 1:16) plot(gasoline$octane, gasoline$NIR[,i], pch = 21, main = paste0("Octane vs.", colnames(gasoline$NIR)[i]))


for(i in 17:32) plot(gasoline$octane, gasoline$NIR[,i], pch = 21, main = paste0("Octane vs.", colnames(gasoline$NIR)[i]))





# ------------------------------------------------------------------------------
# partial least squares regression
# ------------------------------------------------------------------------------

library(pls)



# ----------
# 20 components and leave-one-out (LOO) cross validation
gas1 <- plsr(octane ~ NIR, ncomp = 20, data = gasoline, validation = "LOO")


summary(gas1)



# -->
# RMSEP:  Root Mean Squared Error of Prediction
# two cross-validation estimates: 
#  - CV is the ordinary CV estimate, and adjCV is a bias-corrected CV estimate
#  - For a LOO CV, there is virtually no difference



# explained variance:  first component explains 71.1%  --> 4 comps achieves 95.6%
round( cumsum(explvar(gas1)) / sum(explvar(gas1)), 3)




# ----------
# plot root mean square error of prediction

par(mfrow = c(1,1))

plsCV <- RMSEP(gas1)

plot(plsCV, legend = "topright", type = "o")



# -->
# ncomp = 8 is minimum RMSEP




# ----------
# prediction by 8 components  (note that X and Y both are transformed --> requires predict function)

y <- as.numeric(gasoline$octane)

ypred <- as.numeric(predict(gas1, ncomp = 8))

graphics.off()

plot(y, ypred, main = "Y:prediction by 8 comps   X:Octane")


plot(gas1, ncomp = 8, asp = 1, line = TRUE)





# ----------
# regression coefficients

par(mfrow = c(1,1))

#plot(gas1, plottype = "coef", ncomp=1:13, legendpos = "topright", labels = "numbers", xlab = "nm",
#     main = "coefficient for Octane number by each NIR spectra measurement")

# no legends
plot(gas1, plottype = "coef", ncomp=1:13, labels = "numbers", xlab = "nm",
     main = "coefficient for Octane number by each NIR spectra measurement")

plot(gas1, plottype = "coef", ncomp=1, labels = "numbers", xlab = "nm",
     main = "coefficient for Octane number by each NIR spectra measurement")




# ----------
# correlation plot:
# the correlations between each variable and the selected components
# scatter plots of two sets of scores with concentric circles of radii given by radii.
# Each point corresponds to an X variable.
# The squared distance between the point and the origin equals the fraction of the variance of the variable explained by the components in the panel.
# The default values for radii correspond to 50% and 100% explained variance, respectively.

par(mfrow = c(1,1))

plot(gas1, plottype = "correlation", ncomp=1:8, legendpos = "bottomleft", labels = "numbers")





# ------------------------------------------------------------------------------
# Check for outliers
#   - by pairwise plot of score values
#   - loading plot
# ------------------------------------------------------------------------------

# pairwise plot of the score values for the three first components  --> check for outliers

plot(gas1, plottype = "scores", comps = 1:3, labels = "number")




# ----------
# loading plot
plot(gas1, "loadings", comps = 1:8, legendpos = "topleft")

abline(h = 0)





# ------------------------------------------------------------------------------
# Compute anomaly scores
# ------------------------------------------------------------------------------


N <- length(gasoline$octane)


sig2 <- sum((ypred - y)^2) / N


# anomaly scores
a <- (ypred - y) ^ 2 / sig2




# ----------
graphics.off()
par(mfrow = c(1,2))


plot(ypred ~ y, xlab = "original", ylab = "pred")
abline(0, 1)

plot(a, xlab = "index", ylab = "anomaly score")


# now we set threshold at 95% of scores  --> refer to below for fitting gamma distribution
th <- sort(a)[N * (1 - 0.05)]

lines(0:60, rep(th, length(0:60)), col = "red", lty = 2)




# ----------
graphics.off()

# 13, 17, 47
( idx <- which(a > th) )

colorcode <- rep(3, length(gasoline$octane))

colorcode[idx] <- 1

par(mfrow = c(1,2))

matplot(t(gasoline$NIR[idx,]), type = "l", main = "X: diffuse reflectance measurements (401 type)  Y: NIR spectra for anomaly samples",
        lty = 1, lwd = 2)

barplot(gasoline$octane, horiz = TRUE, col = colorcode, xlim = c(83, 90))



# -->
# spectra is not abnormal, but octane number of 3 gasoline samples are larger or samaller than predicted values




# ------------------------------------------------------------------------------
# Estimate threshold by fitting gamma distribution to anomaly scores
# ------------------------------------------------------------------------------

# moments method: 1st moment and 2nd moment

# mu <- mean(a)
# si <- sum(a^2)/length(a)
# kmo <- (mu / si)^2
# smo <- si^2 / mu



# ----------
# estimate parameter of gamma distribution by maximum likelihood method

ml <- MASS::fitdistr(a, "gamma")
kml <- ml$estimate["shape"]
sml <- 1 / ml$estimate["rate"]

graphics.off()
par(mfrow = c(1,1))
plot(curve(dgamma(x, shape = kml, scale = sml), 0, 20), col = "blue", type = "l")



# ----------
# anomaly score

# 1% percential point
th2 <- qgamma(p = 0.99, shape = kml, scale = sml)



# ----------
graphics.off()
par(mfrow = c(1,2))


plot(ypred ~ y, xlab = "original", ylab = "pred")
abline(0, 1)

plot(a, xlab = "index", ylab = "anomaly score")
lines(0:60, rep(th2, length(0:60)), col = "red", lty = 2)


