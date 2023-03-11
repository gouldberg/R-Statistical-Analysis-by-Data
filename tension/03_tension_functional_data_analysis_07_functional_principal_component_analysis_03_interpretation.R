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



# ----------
# summary(fpca, biplot = TRUE)  




# ------------------------------------------------------------------------------
# For interpretation:  mean, mean + PC, mean - PC
#   - plotting the functional mean trajectory and 
#   - then adding and subtracting a suitable multiple of each PC curve
# ------------------------------------------------------------------------------

fmean <- func.mean(ftension1)


dim(fmean$data)



# Note that the multiple of 3 is arbitrary.
# mean +- 3 * loadings (1st and 2nd loadings)

pc1plus <- fmean$data[1,] + 3 * fpca$rotation$data[1,]

pc1minus <- fmean$data[1,] - 3 * fpca$rotation$data[1,]

pc2plus <- fmean$data[1,] + 3 * fpca$rotation$data[2,]

pc2minus <- fmean$data[1,] - 3 * fpca$rotation$data[2,]




# ----------
graphics.off()

par(mfrow = c(1,2), mar = c(2,2,2,2))

plot(fmean, lwd = 2, main = "Mean (PC1)", ylim = c(-2, 2), col = "red", lty = 1)

lines(ftension1$argvals, pc1plus, lwd = 2, col = gray(0.3), lty = 2)

lines(ftension1$argvals, pc1minus, lwd = 2, col = gray(0.6), lty = 4)

legend("bottomright", legend = c("mean + PC1", "mean - PC1"), lwd = 2, lty = c(2,4), col = c(gray(0.3), gray(0.6)))


plot(fmean, lwd = 2, main = "Mean (PC2)", ylim = c(-2, 2), col = "coral", lty = 1)

lines(ftension1$argvals, pc2plus, lwd = 2, col = gray(0.3), lty = 2)

lines(ftension1$argvals, pc2minus, lwd = 2, col = gray(0.6), lty = 4)

legend("bottomright", legend = c("mean + PC2", "mean - PC2"), lwd = 2, lty = c(2,4), col = c(gray(0.3), gray(0.6)))



# -->
# The 1st component, accounting for 34.14% of the variance:
#   - Participants who score low on this component have a strong initial increase in tension and are highly affected by the peak at around 30s
#     and then their tension drops drastically
#   - Participants who score high on this component have a considerably slow tension increase, are less affected by the tension peak,
#     and remain pretty much constant for the rest of the time (especially in the 40-60s area)
# Thus, the first fPC can be interpreted as "compression/expansion".


# The 2nd component, accounting for 19.19% of the variance, discriminates between tension values at the beginning of the piece (approx. 10-30s)
# and at the end (approx. 60-80s)
#   - Participants who score high on this dimension experience high tension at the beginning and considerably low tension at the end.
#   - For those score low, these tension experiences are reversed.
# We could label this fPC as "edge effects"

