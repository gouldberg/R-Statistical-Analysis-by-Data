setwd("//media//kswada//MyFiles//R//squid_norway")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Squid Norway
# ------------------------------------------------------------------------------
Squid <- read.table(file = "SquidNorway.txt", header = TRUE)


str(Squid)



# ------------------------------------------------------------------------------
# Applying the multiple linear regression model
# ------------------------------------------------------------------------------

# Fit simple multiple linear regression
M1 <- glm(d15N ~ ML + Lat, data = Squid)


summary(M1)



# ------------------------------------------------------------------------------
# model validation:  residuals versus fitted values and mantle length
# ------------------------------------------------------------------------------

E1 <- rstandard(M1)
F1 <- fitted(M1)

par(mfrow = c(1,2), mar = c(5,5,3,3))
plot(x = F1, y = E1, cex.lab = 1.5, xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, v = 0)

plot(x = Squid$ML, y = E1, cex.lab = 1.5, xlab = "Mantel length", ylab = "Residuals")
abline(h = 0)
L1 <- loess(E1 ~ Squid$ML)
I1 <- order(Squid$ML)
lines(x = sort(Squid$ML), y = fitted(L1)[I1], lwd = 3)



# -->
# No obvious heterogeneity is present by residuals versus fitted values plot
# by residuals versus mantle length plot, our first impression is that there is non non-linear pattern,
# but we are not 100% convinced that this is the case.

# From plot of residuals versus mantel length, now we have discovered that there are nonlinear patterns in the residuals we need to solve problme.
# Factors such as missing covariate, outliers, spatial or temporal correlation, zero inflation, collinearity, a missing interaction,
#"wrong link function, or some od behaviour of the model may all cause nonlinear effects in residuals.


# ----------
# applying a smoothing model on these residuals for validation
# intercep is removed because the residuals have a mean of 0.
T1 <- gam(E1 ~ -1 + s(ML), data = Squid)
summary(T1)

par(mfrow=c(1,1), mar = c(5,5,3,3))
plot(T1, cex.lab = 1.5)
abline(h = 0, lty = 2)



# -->
# Both the numerical output and the smoother indicate that there is a significant mantle length effect in the residuals.
# The smoother still explains 15.4% of the variation in the residuals.


