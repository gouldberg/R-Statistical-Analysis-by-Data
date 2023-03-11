setwd("//media//kswada//MyFiles//R//tension")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  tension
# ------------------------------------------------------------------------------

data("tension", package = "MPsychoR")


str(tension)



# ----------
# convert to fdata object
library(fda.usc)

# tension time series
tension1 <- as.matrix(tension[, 1:800])

cond <- tension$condition


ftension <- fdata(tension1, argvals = seq(1, 80, length.out = 800),
                  names = list(main = "Music tension", xlab = "Time (sec)", ylab = "Tension"))


ftension



# ------------------------------------------------------------------------------
# Functional Principal Component Analysis
#   - The resulting fPCs are again functions
# ------------------------------------------------------------------------------

# fit a 2D-fPCA on the music tension data.
fpca <- fdata2pc(ftension1, ncomp = 2)




# ------------------------------------------------------------------------------
# PCA scores
# ------------------------------------------------------------------------------

# PCA scores:  subject * PC ( = time points)
dim(fpca$x)



# ----------
# 1st and 2nd PCA
head(fpca$x[,1:2])


summary(fpca$x[,1:2])



# ------------------------------------------------------------------------------
# loadings
# ------------------------------------------------------------------------------

# loadings for 1st and 2nd component

head(t(fpca$rotation$data[1:2,]))




# ------------------------------------------------------------------------------
# plot loadings along time point for for each component
# ------------------------------------------------------------------------------

pcscores <- fpca$x[,1:2]

cols <- c("darkgray", "black", "blue")[as.numeric(cond)]



graphics.off()
par(mfrow = c(1,2), mar = c(2,2,2,2))



# ----------
# loadings
plot(fpca$rotation[1,]$argvals, fpca$rotation$data[1,], type = "l", 
     ylim = c(-0.3, 0.3), xlab = "Time",  ylab = "Loadings", main = "Functional PCA Loadings",
     lwd = 2, col = "red", cex.main = 2)
abline(h = 0, lty = 2, col = "gray")

lines(fpca$rotation$argvals, fpca$rotation$data[2,], type = "l", col = "coral", lwd = 2, lty = 2)

legend("topright", legend = c("PC1", "PC2"), lty = c(1,2), col = c("red", "coral"), cex = 1.2)



# -->
# NOTE THAT PCs are deviation from mean trajectory, not trajectory itself




# ----------
# scores
plot(pcscores, type = "n", asp = 1, main = "Functional PC Scores", cex.main = 2)
abline(h = 0, v = 0, lty = 2, col = "gray")

text(pcscores, col = cols, cex = 1.2)

legend("topright", legend = c("Auditory", "Visual", "Auditory & Visual"), text.col = c("darkgray", "black", "blue"), cex = 1.2)



# -->
# A clear separation between participants in the visual condition and participants in the auditory/auditory-vidual conditions.
# The scores for auditory vs. auditory-visual are poorly separated in two dimensions.

# These findings substantiate the functional ANOVA and regression results in previous analysis.

