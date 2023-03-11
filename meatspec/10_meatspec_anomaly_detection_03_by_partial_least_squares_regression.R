
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\05_anomaly_detection\\02_異常検知_入力出力データ\\meatspec")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# data:  meatspec
# ------------------------------------------------------------------------------


# data(meatspec, package="faraway")

meatspec <- read.csv("meatspec.txt", header = T, sep = "\t")


names(meatspec)


# 100-channel spectrum of absorbances for 215 meat samples
dim(meatspec)




# ------------------------------------------------------------------------------
# partial least squares regression
# ------------------------------------------------------------------------------


# select all variables

cc <- paste0("V", 1:100)



# need scaling
Xc <- scale(meatspec[,cc])


psych::describe(Xc)


head(Xc)




# y:  fat
meatspec_s <- data.frame(Xc, fat = meatspec[,"fat"])




# ----------
library(pls)


set.seed(123)


plsmod <- plsr(fat ~ ., data = meatspec_s, ncomp = 50, validation = "CV")


# explained variance:  first component explains 98.6%  --> 4 comps achieves 100%
round( cumsum(explvar(plsmod)) / sum(explvar(plsmod)), 3)




# ----------
# plot root mean square error of prediction

plsCV <- RMSEP(plsmod, estimate = "CV")

plot(plsCV, type = "o")



# -->
# ncomp = 14 is minimum RMSEP




# ----------
# prediction by 14 components  (note that X and Y both are transformed --> requires predict function)

y <- as.numeric(meatspec_s[,"fat"])

ypred <- as.numeric(predict(plsmod, ncomp = 14))

plot(y, ypred, main = "Y:prediction by 14 comps   X:standardized fat")

plot(plsmod, ncomp = 14, asp = 1, line = TRUE)




# ----------
# coefficient used for 14 component model

coef(plsmod, ncomp = 14)

coefplot(plsmod, ncomp = 14, xlab = "Frequency")


plot(plsmod, plottype = "coef", ncomp = 1:14, legendpos = "bottomleft", xlab = "Frequency")




# ----------
# correlation plot:
# the correlations between each variable and the selected components
# scatter plots of two sets of scores with concentric circles of radii given by radii.
# Each point corresponds to an X variable.
# The squared distance between the point and the origin equals the fraction of the variance of the variable explained by the components in the panel.
# The default values for radii correspond to 50% and 100% explained variance, respectively.

par(mfrow = c(1,1))

plot(plsmod, plottype = "correlation", ncomp=1:14, legendpos = "bottomleft", labels = "numbers")




# ------------------------------------------------------------------------------
# Check for outlisers
#   - by pairwise plot of score values
#   - loading plot
# ------------------------------------------------------------------------------

# pairwise plot of the score values for the three first components  --> check for outliers

plot(plsmod, plottype = "scores", comps = 1:4, labels = "number")




# ----------
# loading plot
plot(plsmod, "loadings", comps = 1:14, legendpos = "topleft")

abline(h = 0)





# ------------------------------------------------------------------------------
# Compute anomaly scores
# ------------------------------------------------------------------------------


N <- nrow(meatspec_s)


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

lines(0:215, rep(th, length(0:215)), col = "red", lty = 2)




# ----------
graphics.off()

par(mfrow = c(1,1))

( idx <- which(a > th) )


matplot(t(meatspec[idx,1:100]), type = "l", main = "X: 100-channel  Y: anomaly meat samples' fat", 
        lty = 1, lwd = 2)





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
lines(0:215, rep(th2, length(0:215)), col = "red", lty = 2)




# ----------
graphics.off()

par(mfrow = c(1,1))

( idx <- which(a > th2) )

matplot(t(meatspec[idx,1:100]), type = "l", main = "X: 100-channel  Y: anomaly meat samples' fat", 
        lty = 1, lwd = 2)



