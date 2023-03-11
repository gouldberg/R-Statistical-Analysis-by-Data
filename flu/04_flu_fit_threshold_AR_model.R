setwd("//media//kswada//MyFiles//R//flu")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  flu
# ------------------------------------------------------------------------------

data(flu, package = "astsa")

str(flu)

flu




# ------------------------------------------------------------------------------
# Threshold modeling (TARMA)
#   - The basic idea is that of fitting local linear ARMA models
# ------------------------------------------------------------------------------

dflu <- diff(flu)


acf2(dflu)



# ----------
# Lag 1 - 4
Z <- ts.intersect(dflu, stats::lag(dflu, -1), stats::lag(dflu, -2), stats::lag(dflu, -3), stats::lag(dflu, -4))



# ----------
# 2 regime by threshold = 0.05
# We chose the model with a threshold of 0.05 because the residuals diagnostics showed no obvious departure from the model assumption
# and the model with a threshold of 0.04 still had some correlation left in the residuals and there was more than one outliner.

thrsh <- 0.05
# thrsh <- 0.04


# z[,2] is lag(dflu, -1)
ind1 <- ifelse(Z[,2] < thrsh, 1, NA)

ind2 <- ifelse(Z[,2] < thrsh, NA, 1)


sum(ind2 == 1, na.rm = TRUE)


# -->
# threshold of 0.05 was exceeded 17 times.




# ----------
# x1 and x2: differenced series
X1 <- Z[,1] * ind1

X2 <- Z[,1] * ind2


head(X1, 100)


head(X2, 100)




# ----------
# regression by each regime with lag 1 - 4

summary(fit1 <- lm(X1 ~ Z[,2:5]))

summary(fit2 <- lm(X2 ~ Z[,2:5]))



# -->
# where sigma1 = 0.046 and sigma2 = 0.072




# ------------------------------------------------------------------------------
# one-month-ahead prediction for dflu
#
#   - NOTE THAT not for flu itself
# ------------------------------------------------------------------------------


# design matrix

D <- cbind(rep(1, nrow(Z)), Z[,2:5])




# ----------
# get predictions
p1 <- D %*% coef(fit1)

p2 <- D %*% coef(fit2)



# ----------
# error sigma
prde1 <- sqrt(sum(resid(fit1)^2) / df.residual(fit1))

prde2 <- sqrt(sum(resid(fit2)^2) / df.residual(fit2))




# ----------
prd <- ifelse(Z[,2] < thrsh, p1, p2)

prde <- ifelse(Z[,2] < thrsh, prde1, prde2)

tx <- time(dflu)[-c(1:4)]
xx <- c(tx, rev(tx))
yy <- c(prd - 2 * prde, rev(prd + 2 * prde))



# ----------
# (+): First differenced U.S. monthly pneumonia and influenza deaths
# one-month-ahead predictions (solid line) with +-2 prediction error bounds
# horizontal line is the threshold

plot(dflu, ylim = c(-0.5, 0.5), type = "p", pch = 3)

lines(prd)

polygon(xx, yy, border = 8, col = gray(0.6, alpha = 0.25))

abline(h = 0.05, col = 4, lty = 6)



# -->
# The model does extremely well at predicting a flu epidemic;
# The peak at 1976, however, was missed by thie model.
