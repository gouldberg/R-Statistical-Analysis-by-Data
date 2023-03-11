setwd("//media//kswada//MyFiles//R//abc")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  ABC
# ------------------------------------------------------------------------------

data("ABC", package = "MPsychoR")

str(ABC)

dim(ABC)

car::some(ABC)



# ----------
# convert factor to numeric
ABC2 <- rapply(ABC[,6:11], f = as.numeric, classes = "factor", how = "replace")

str(ABC2)



# ------------------------------------------------------------------------------
# Illustrate that the principle of constructing a regression biplot holds for the PCA biplot as well.
# ------------------------------------------------------------------------------

# Extract PC scores (PC1 and PC2 are predictors)
X <- pca_abc2$x[, 1:2]

# standardize variables
Y <- scale(ABC2)


# fit regression
fitlms <- lm(Y ~ -1 + X)


# The regression coefficients should be the same as the lodings
round(coef(fitlms), 3)

round(t(pca_abc2$rotation[,1:2]), 3)



# ----------
# Orthogonal projections on AE (affective empathy) axis
plot(X[,1], X[,2], pch = 20, xlab = "PC1", ylab = "PC2", col = "darkblue", asp = 1, main = "Biplot Axis",  xlim = c(-3.2, 3.2))
text(X[,1], X[,2], labels = rownames(X), cex = 0.7, pos = 3, col = "darkblue")
abline(h = 0, v = 0, lty = 2, col = "gray")
calAE <- calibrate(fitlms$coef[,"pricing"], Y[,"pricing"], tm = seq(-2, 2, by = 0.5), Fr = X, dp = TRUE, 
                   axiscol = "brown", axislab = "pricing", labpos = 3, verb = FALSE)



# R^2 values for response pricing is high but only 0.716
summary(fitlms)
R2vec <- sapply(summary(fitlms), "[[", "r.squared")
round(R2vec, 3)

