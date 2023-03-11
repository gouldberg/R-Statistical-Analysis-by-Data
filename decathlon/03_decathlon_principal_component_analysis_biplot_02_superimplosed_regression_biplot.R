setwd("//media//kswada//MyFiles//R//decathlon")

packages <- c("dplyr", "MPsychoR", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  decathlon
# ------------------------------------------------------------------------------

data("decathlon", package = "FactoMineR")

str(decathlon)

dim(decathlon)


decathlon



# ------------------------------------------------------------------------------
# Illustrate that the principle of constructing a regression biplot holds for the PCA biplot as well.
# ------------------------------------------------------------------------------

# Extract PC scores (PC1 and PC2 are predictors)
( X <- pca_dec2$x[, 1:2] )

# standardize variables
Y <- scale(decathlon[,1:10])


# fit regression
fitlms <- lm(Y ~ -1 + X)


# The regression coefficients should be the same as the loadings
round(coef(fitlms), 3)

round(t(pca_dec2$rotation[,1:2]), 3)



# ----------
R2vec <- sapply(summary(fitlms), "[[", "r.squared")

sort(round(R2vec, digits = 3), decreasing = TRUE)



# ----------
# Orthogonal projections on 400m  (R^2 is largst 0.786)
# R^2 values for response 400m is high and we can trust the projections on 400m axis. 

plot(X[,1], X[,2], pch = 20, xlab = "PC1", ylab = "PC2", col = "darkblue", asp = 1, main = "Biplot Axis",  xlim = c(-3.2, 3.2))
text(X[,1], X[,2], labels = rownames(X), cex = 0.7, pos = 3, col = "darkblue")
abline(h = 0, v = 0, lty = 2, col = "gray")
calibrate::calibrate(fitlms$coef[,"400m"], Y[,"400m"], tm = seq(-2, 2, by = 0.5), Fr = X, dp = TRUE, 
                   axiscol = "brown", axislab = "400m", labpos = 3, verb = FALSE)




# ----------
# Orthogonal projections on Pole.vault axis  (R^2 is smallest 0.035)
# plot(X[,1], X[,2], pch = 20, xlab = "PC1", ylab = "PC2", col = "darkblue", asp = 1, main = "Biplot Axis",  xlim = c(-3.2, 3.2))
# text(X[,1], X[,2], labels = rownames(X), cex = 0.7, pos = 3, col = "darkblue")
# abline(h = 0, v = 0, lty = 2, col = "gray")
calibrate::calibrate(fitlms$coef[,"Pole.vault"], Y[,"Pole.vault"], tm = seq(-2, 2, by = 0.5), Fr = X, dp = TRUE, 
                   axiscol = "brown", axislab = "Pole.vault", labpos = 3, verb = FALSE)


