setwd("//media//kswada//MyFiles//R//orange")

packages <- c("dplyr", "MPsychoR", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  orange
# ------------------------------------------------------------------------------

orange <- read.table("orange.csv", header=TRUE, sep=";", dec=".", row.names=1)

str(orange)

dim(orange)


orange




# ------------------------------------------------------------------------------
# Illustrate that the principle of constructing a regression biplot holds for the PCA biplot as well.
# ------------------------------------------------------------------------------

# Extract PC scores (PC1 and PC2 are predictors)
( X <- pca_or2$x[, 1:2] )

# standardize variables
Y <- scale(orange[,1:7])


# fit regression
fitlms <- lm(Y ~ -1 + X)


# The regression coefficients should be the same as the loadings
round(coef(fitlms), 3)

round(t(pca_or2$rotation[,1:2]), 3)



# ----------
R2vec <- sapply(summary(fitlms), "[[", "r.squared")

sort(round(R2vec, digits = 3), decreasing = TRUE)



# ----------
# Orthogonal projections on Odour.typicality axis  (R^2 is largst 0.9834)
# R^2 values for response Odour.typicality is very high (almost 1) and we can trust the projections on the Odour.typicality axis. 

plot(X[,1], X[,2], pch = 20, xlab = "PC1", ylab = "PC2", col = "darkblue", asp = 1, main = "Biplot Axis",  xlim = c(-3.2, 3.2))
text(X[,1], X[,2], labels = rownames(X), cex = 0.7, pos = 3, col = "darkblue")
abline(h = 0, v = 0, lty = 2, col = "gray")
calibrate::calibrate(fitlms$coef[,"Odour.typicality"], Y[,"Odour.typicality"], tm = seq(-2, 2, by = 0.5), Fr = X, dp = TRUE, 
                   axiscol = "brown", axislab = "Odour.typicality", labpos = 3, verb = FALSE)


#biplot(pca_or2, pc.biplot = TRUE, cex = c(1, 1), col = c("black", "blue"),
#       #       arrow.len = 0.05, asp = 1,
#       main = "Biplot (Standardized)", cex.axis = 0.8)



# ----------
# Orthogonal projections on Itensity.of.taste axis  (R^2 is smallest 0.4094)
# plot(X[,1], X[,2], pch = 20, xlab = "PC1", ylab = "PC2", col = "darkblue", asp = 1, main = "Biplot Axis",  xlim = c(-3.2, 3.2))
# text(X[,1], X[,2], labels = rownames(X), cex = 0.7, pos = 3, col = "darkblue")
# abline(h = 0, v = 0, lty = 2, col = "gray")
calibrate::calibrate(fitlms$coef[,"Intensity.of.taste"], Y[,"Intensity.of.taste"], tm = seq(-2, 2, by = 0.5), Fr = X, dp = TRUE, 
                   axiscol = "brown", axislab = "Intensity.of.taste", labpos = 3, verb = FALSE)





