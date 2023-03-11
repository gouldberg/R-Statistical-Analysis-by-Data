setwd("//media//kswada//MyFiles//R//squid_norway")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Squid Norway
# ------------------------------------------------------------------------------
Squid <- read.table(file = "SquidNorway.txt", header = TRUE)


str(Squid)



# ------------------------------------------------------------------------------
# Applying an additive model
# ------------------------------------------------------------------------------
M2 <- gam(d15N ~ Lat + s(ML), data = Squid)
summary(M2)


# -->
# The effect of latitude is positive and weakly significant at the 5% level.
# The smoother has 2.57 degrees of freedom and is highly significant.
# The R^2 (deviance explained) is 59.2% and the estimated variance of the residuals is 0.382.
# The GCV score was used to find the optimal amount of smoothing of the thin plate regression spline (default of GAM).



# ------------------------------------------------------------------------------
# The estimated smoother
# ------------------------------------------------------------------------------
plot(M2, resid = TRUE, pch = 16, cex = 0.5)
abline(h = 0, lty = 2)


# -->
# The smoother suggests that there is a non-linear pattern.



# ------------------------------------------------------------------------------
# Model validation
# ------------------------------------------------------------------------------
E2 <- resid(M2)
F2 <- fitted(M2)

par(mfrow = c(2,3), mar = c(5,5,3,3))
plot(x = F2, y = E2, xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, lty = 2)

plot(x = Squid$ML, y = E2, xlab = "ML", ylab = "Residuals")
abline(h = 0, lty = 2)

plot(x = Squid$Lat,  y = E2, xlab = "Latitude", ylab = "Residuals")
abline(h = 0, lty = 2)

plot(x = Squid$Depth,  y = E2, xlab = "Depth", ylab = "Residuals")
abline(h = 0, lty = 2)

hist(E2, xlab = "", ylab ="", breaks = 10)

plot(sort(E2), type = "h", xlab = "Sorted residuals", ylab = "Residuals")


# -->
# Any patterns in these graphs means that the GAM is unsuitable.

# The multiple linear regression model has a clear non-linear residual pattern with mantle length, an indication that the model
# is not suitable.
# The additive model did not show residual patterns with mantle length or any of the other covariates, and therefore is the
# preferred model.
# The cross-validation process carried out by the gam function revealed 2.577 degrees of freedom.
# A suitable multipe linear regression model would have 1 degree of freedom.



# ----------
# Compare models
AIC(M1, M2)


# -->
# indicating that the additive model is superior.


# ----------
# F-test altough it is difficult to specify the null hypothesis.
# In general, p-values for smoother betwen 0.001 and 0.1 are inconclusive and most likely indicate a very weak effect.
anova(M1, M2, test = "F")


# -->
# indicating that the additive model is better.


