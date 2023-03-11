setwd("//media//kswada//MyFiles//R//heger_priede")

packages <- c("dplyr", "lattice", "mgcv")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Heger Priede
# ------------------------------------------------------------------------------
BL <- read.table(file = "//media//kswada//MyFiles//references//ZuurBeginnersGuideToGeneralizedAdditiveModelsWithR//HegerPriede.txt", header = TRUE)


str(BL)

names(BL)



# ------------------------------------------------------------------------------
# relationship between Sources and Depth
# ------------------------------------------------------------------------------

# The data were sampled at 14 stations, but in the beginning our analysis we will ignore any potential station effects.

# scatterplot of depth in metres versus bioluminescence counts per cubic meter (called "Sources" in the data file)

# Scale Depth
BL$DepthOriginal <- BL$Depth
BL$Depth <- BL$Depth/max(BL$Depth)

par(mfrow=c(1,2))
plot(x = BL$DepthOriginal, y = BL$Sources, xlab = "Depth", ylab ="Sources",  cex.lab = 1.5)
plot(x = BL$Depth, y = BL$Sources, xlab = "Scaled depth", ylab ="Sources",  cex.lab = 1.5, cex = 0.7,  pch = 16, col = grey(0.5))



# ------------------------------------------------------------------------------
# using the function gam in mgcv
#   - This applies a penalized regression spline, and cross validation is used to estimate the amount of smoothing.
# ------------------------------------------------------------------------------
G1 <- mgcv::gam(Sources ~ s(Depth), data = BL)

summary(G1)

plot(G1)



# ----------
# We can shift the smoother by 16.745 and superimpose the data
plot(G1, shift = 16.7455, ylim = c(-10,100), lwd =3, rug = FALSE, cex.lab = 1.5)
points(x = BL$Depth, y = BL$Sources, cex = 0.5, pch = 16, col = grey(0.5))



# ------------------------------------------------------------------------------
# regression spline with fixed degrees of freedom
# ------------------------------------------------------------------------------
# The smoother used 4 degrees of freedom (the intercept is dropped from the smoother due to identifiability, and the constraints on the regression
# parameters at the knots ensure that the degrees of freedom is 4)
gam(Sources ~ s(Depth, fx = TRUE, k = 5), data = BL)



# ------------------------------------------------------------------------------
# cr:  apply penalized cubic regression spline
# ------------------------------------------------------------------------------
# a penalized cubic regression spline
G2 <-  gam(Sources ~  s(Depth, bs = "cr"), data = BL)


# the maximum number of knots is 20
G2A <- gam(Sources ~ s(Depth, bs = "cr", k =20), data = BL)


# set the degrees of freedom to 6 and keep it fixed (no cross-validation)
G2B <- gam(Sources ~ s(Depth, bs = "cr", k =7, fx = TRUE), data = BL)


# increase the weight of the penalty term so that slightly smoother curves are obtained
G2C <- gam(Sources ~ s(Depth, bs = "cr", k =20), gamma = 1.4, data = BL)



# ------------------------------------------------------------------------------
# fitted values, regression coefficients, and standard errors for the fitted values
# ------------------------------------------------------------------------------
# obtain the fitted values of the model
fitted(G1)



# ----------
# regression coefficients
coefficients(G1)



# ----------
# predict with function type = "lpmatrix" gives the covariates included in the linear regression model
X  <- predict(G1, type = "lpmatrix")


# ----------
# fitted values = multiply the covariates with the estimated regression parameters
mu <- X %*% coef(G1)


# --> mu and fitted(G1) are identical.


# ----------
# covariance matrix
X%*% G1$Vp %*% t(X)



# ----------
# standard errors for the fitted values = sqrt(diag(covariance matrix))
sqrt(diag(X%*% G1$Vp %*% t(X)))


# standard errors by predict
predict(G1, se = TRUE)



# ------------------------------------------------------------------------------
# take into account other covariate: Station
# ------------------------------------------------------------------------------
# plot residuals vs. fitted values
E1 <- resid(G1)
F1 <- fitted(G1)

plot(x = F1, y = E1, cex = 0.7, pch = 16, xlab = "Fitted values", ylab = "Residuals",cex.lab = 1.5)
abline(h = 0, lty = 2)


# -->
# The resulting graph shows clear heterogeneity. The graph shows an unusual double-cone pattern.
# We do not appear to have the problem of negative fitted values.



# ----------
# Residuals conditional on station.
boxplot(E1 ~ Station,  data = BL, xlab = "Station", ylab = "Residuals")
abline(h = 0, lty = 2)


# -->
# Note that the residuals of some stations are nearly all above the zero line, or below the zero line.
# Note that there are clear residuals patterns that differ per station.



# ----------
# Residuals plotted versus depth for each station for the GAM using only depth as smoother
xyplot(E1 ~ Depth | factor(Station), data = BL, xlab = list(label = "Depth", cex = 1.5), ylab = list(label = "Residuals", cex = 1.5),
       strip = strip.custom(bg = 'white', par.strip.text = list(cex = 1.2)),
       panel = function(x,y){
         panel.points(x, y, pch = 16, cex = 0.7, col = 1)
         panel.abline(h = 0, lty = 2)
       }
)


# ----------
# An eddy is a circular movement of ocean water with a diameter between 100 and 200 km.
# We could hypothesize that the sources/depth relationship differs depending on whether there is an eddy present.
xyplot(Sources ~ Depth | factor(Eddy), groups = factor(Station), type = "l",  col = 1,  pch = 16, data = BL,
       xlab = list(label = "Depth", cex = 1.5), ylab = list(label = "Sources", cex = 1.5))


# -->
# The left panel shows the stations where there are no eddies, and the right panel shows the stations with eddies.



# ------------------------------------------------------------------------------
# model comparison
# ------------------------------------------------------------------------------
G3 <- gam(Sources ~ s(Depth) + factor(Station),  data = BL)

G4 <- gam(Sources ~ s(Depth, by = factor(Eddy)) + factor(Station),  data = BL)

AIC(G3, G4)



# -->
# Clearly the model with two smoothers (one per eddy) is better.


par(mfrow = c(1,2))
plot(G4)



# -->
# The smoothers have different patterns, especially at the middle of the depth gradient, and both shows a bump.
# Note that the confidence interval for the second smoother is wider for greater depths.
# This is probably due to limited number of observations at these depths when there are eddies.
