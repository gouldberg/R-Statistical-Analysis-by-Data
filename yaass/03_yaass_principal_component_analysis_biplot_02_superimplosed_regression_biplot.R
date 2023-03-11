setwd("//media//kswada//MyFiles//R//yaass")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  yaass
# ------------------------------------------------------------------------------

data("yaass", package = "MPsychoR")

str(yaass)



# ------------------------------------------------------------------------------
# Illustrate that the principle of constructing a regression biplot holds for the PCA biplot as well.
# ------------------------------------------------------------------------------

# Extract PC scores (PC1 and PC2 are predictors)
X <- pca_yaass2$x[, 1:2]

# standardize variables
Y <- scale(yaass[,1:5])


# fit regression
fitlms <- lm(Y ~ -1 + X)


# The regression coefficients should be the same as the lodings
round(coef(fitlms), 3)

round(t(pca_yaass2$rotation[,1:2]), 3)



# ----------
# Orthogonal projections on AE (affective empathy) axis
plot(X[,1], X[,2], pch = 20, xlab = "PC1", ylab = "PC2", col = "darkblue", asp = 1, main = "Biplot Axis",  xlim = c(-3.2, 3.2))
text(X[,1], X[,2], labels = rownames(X), cex = 0.7, pos = 3, col = "darkblue")
abline(h = 0, v = 0, lty = 2, col = "gray")
calAE <- calibrate(fitlms$coef[,"AE"], Y[,"AE"], tm = seq(-2, 2, by = 0.5), Fr = X, dp = TRUE, 
                   axiscol = "brown", axislab = "AE", labpos = 3, verb = FALSE)



# R^2 values for response AE is very high (almost 1) and we can trust the projections on the AE axis. 
# This implies that by means of these two PCs, variation in AE is fully explained.
summary(fitlms)
R2vec <- sapply(summary(fitlms), "[[", "r.squared")
round(R2vec[2], 3)

