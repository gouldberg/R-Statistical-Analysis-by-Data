setwd("//media//kswada//MyFiles//R//sunspotz")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  sunspotz
# ------------------------------------------------------------------------------

data(sunspotz, package = "astsa")

str(sunspotz)

head(sunspotz)



# ------------------------------------------------------------------------------
# Threshold modeling (TARMA)
#   - The basic idea is that of fitting local linear ARMA models
# ------------------------------------------------------------------------------

dsun <- diff(sunspotz)



# ----------
# Lag 1 - 4
Z <- ts.intersect(dsun, stats::lag(dsun, -1), stats::lag(dsun, -2), stats::lag(dsun, -3), stats::lag(dsun, -4))



# ----------
# 2 regime by threshold = 5

thrsh <- 5


ind1 <- ifelse(Z[,2] < thrsh, 1, NA)

ind2 <- ifelse(Z[,2] < thrsh, NA, 1)

X1 <- Z[,1] * ind1

X2 <- Z[,1] * ind2



# ----------
# regression by each regime with lag 1 - 4

summary(fit1 <- lm(X1 ~ Z[,2:5]))

summary(fit2 <- lm(X2 ~ Z[,2:5]))



# -->
# fit1 is better (samller residual standard error)


# ----------
plot(fit1)

plot(fit2)



# ----------
# design matrix
D <- cbind(rep(1, nrow(Z)), Z[,2:5])



# ----------
# get predictions
p1 <- D %*% coef(fit1)

p2 <- D %*% coef(fit2)



# ----------
prde1 <- sqrt(sum(resid(fit1)^2) / df.residual(fit1))

prde2 <- sqrt(sum(resid(fit2)^2) / df.residual(fit2))



# ----------
prd <- ifelse(Z[,2] < thrsh, p1, p2)

prde <- ifelse(Z[,2] < thrsh, prde1, prde2)

tx <- time(dsun)[-c(1:4)]
xx <- c(tx, rev(tx))
yy <- c(prd - 2 * prde, rev(prd + 2 * prde))



# ----------
# (+): First differenced
# one-month-ahead predictions (solid line) with +-2 prediction error bounds
# horizontal line is the threshold

plot(dsun, type = "p", pch = 3)
lines(prd)
polygon(xx, yy, border = 8, col = gray(0.6, alpha = 0.25))
abline(h = thrsh, col = 4, lty = 6)



# -->
# The model does extremely well at predicting
