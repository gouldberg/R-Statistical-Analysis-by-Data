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
# Smoothed curves for the means and median
# ------------------------------------------------------------------------------

# split the music tension data according to the conditions

# fsplit <- split.fdata(ftension1, cond)
fsplit <- split(ftension1, cond)



# compute functional means and functional medians
Amean <- func.mean(fsplit$Auditory)

Vmean <- func.mean(fsplit$Visual)

AVmean <- func.mean(fsplit$AuditoryVisual)



Amedian <- func.med.FM(fsplit$Auditory)

Vmedian <- func.med.FM(fsplit$Visual)

AVmedian <- func.med.FM(fsplit$AuditoryVisual)



# ----------
graphics.off()

par(mfrow = c(2,1), mar = c(2,2,2,2))

plot(Amean, lwd = 2, main = "Mean Tension Trajectories", ylim = c(-2.5, 2.5))
abline(h = 0, lty = 2, col = "darkgray")

lines(Vmean, col = "coral", lwd = 2)
lines(AVmean, col = "cadetblue", lwd = 2)
legend("bottomright", col = c(1, "coral", "cadetblue"), lty = 1, legend = c("Auditory", "Visual", "Auditory & Visual"), cex = 0.5)

plot(Amedian, lwd = 2, main = "Median Tension Trajectories", ylim = c(-2.5, 2.5))
abline(h = 0, lty = 2, col = "darkgray")

lines(Vmedian, col = "coral", lwd = 2)
lines(AVmedian, col = "cadetblue", lwd = 2)
legend("bottomright", col = c(1, "coral", "cadetblue"), lty = 1, legend = c("Auditory", "Visual", "Auditory & Visual"), cex = 0.5)


