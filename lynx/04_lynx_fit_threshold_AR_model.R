setwd("//media//kswada//MyFiles//R//lynx")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  lynx
# ------------------------------------------------------------------------------

data(lynx, package = "datasets")


str(lynx)


lynx



# ----------
lynxl <- log10(lynx)

dlynxl <- diff(log10(lynx))




# ------------------------------------------------------------------------------
# Threshold modeling (TARMA)
#   - The basic idea is that of fitting local linear ARMA models
# ------------------------------------------------------------------------------

# Lag 1 - 4
Z <- ts.intersect(lynxl, stats::lag(lynxl, -1), stats::lag(lynxl, -2), stats::lag(lynxl, -3), stats::lag(lynxl, -4))



# ----------
# 2 regime by threshold = 2.5

thrsh <- 2.5


# z[,2] is lag(dflu, -1)
ind1 <- ifelse(Z[,2] < thrsh, 1, NA)

ind2 <- ifelse(Z[,2] < thrsh, NA, 1)


sum(ind2 == 1, na.rm = TRUE)


# -->
# threshold of 2.5 was exceeded 84 times.




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





# ------------------------------------------------------------------------------
# one-step-ahead prediction
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

tx <- time(lynxl)[-c(1:4)]
xx <- c(tx, rev(tx))
yy <- c(prd - 2 * prde, rev(prd + 2 * prde))



# ----------
# (+): First differenced U.S. monthly pneumonia and influenza deaths
# one-month-ahead predictions (solid line) with +-2 prediction error bounds
# horizontal line is the threshold

par(mfrow = c(1,1))

plot(lynxl, ylim = c(1.5, 4.5), type = "p", pch = 3)

lines(prd)

polygon(xx, yy, border = 8, col = gray(0.6, alpha = 0.25))

abline(h = thrsh, col = 4, lty = 6)


