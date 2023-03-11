setwd("//media//kswada//MyFiles//R//tension")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  tension
# ------------------------------------------------------------------------------

data("tension", package = "MPsychoR")


str(tension)


dim(tension)



# -----------
tens <- t(as.matrix(tension[, 1:800]))


# nonparametric smoothed data
# tens <- t(ftensionNP$fdata.est$data)



# ------------------------------------------------------------------------------
# standardize individual series
# ------------------------------------------------------------------------------


std <- diag(1 / sqrt(diag(cov(tens))))


tens <- as.matrix(tens) %*% std




# ------------------------------------------------------------------------------
# Assess unidimensionality
#   - fitting a 2-dimensional Princals in order to get a picture of item associations in a 2D space.
# ------------------------------------------------------------------------------

library(Gifi)


prin <- princals(tens)


prin



# ----------
# plot loadings

graphics.off()

par(mfrow = c(1,2))

plot(prin, main = "Loadings")



# ----------
# PCA scores in 2D by functional PCA
plot(pcscores, type = "n", asp = 1, main = "Functional PC Scores", cex.main = 2)
abline(h = 0, v = 0, lty = 2, col = "gray")

text(pcscores, col = cols, cex = 1.2)

legend("topright", legend = c("Auditory", "Visual", "Auditory & Visual"), text.col = c("darkgray", "black", "blue"), cex = 1.2)





