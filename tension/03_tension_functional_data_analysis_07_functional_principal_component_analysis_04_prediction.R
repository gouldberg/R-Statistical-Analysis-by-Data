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
# again plot loadings and scores
# ------------------------------------------------------------------------------

pcscores <- fpca$x[,1:2]

cols <- c("darkgray", "black", "blue")[as.numeric(cond)]

graphics.off()
par(mfrow = c(1,2), mar = c(2,2,2,2))

plot(fpca$rotation[1,]$argvals, fpca$rotation$data[1,], type = "l", 
     ylim = c(-0.3, 0.3), xlab = "Time",  ylab = "Loadings", main = "Functional PCA Loadings",
     lwd = 2, col = "red", cex.main = 2)
abline(h = 0, lty = 2, col = "gray")
lines(fpca$rotation$argvals, fpca$rotation$data[2,], type = "l", col = "coral", lwd = 2, lty = 2)
legend("topright", legend = c("PC1", "PC2"), lty = c(1,2), col = c("red", "coral"), cex = 1.2)


plot(pcscores, type = "n", asp = 1, main = "Functional PC Scores", cex.main = 2)
abline(h = 0, v = 0, lty = 2, col = "gray")
text(pcscores, col = cols, cex = 1.2)
legend("topright", legend = c("Auditory", "Visual", "Auditory & Visual"), text.col = c("darkgray", "black", "blue"), cex = 1.2)



# -->
# note for 17:  scores for PC1 is almost 0 but scores for PC2 is high
# note for 14, 18:  scores for PC1 is higth



# ------------------------------------------------------------------------------
# plot trajectories of original smoothed and PC scores * loadings 
# ------------------------------------------------------------------------------

fpca_m <- fdata2pc(ftension1, ncomp = 10)


pc1_loadings <- fpca_m$rotation$data[1,]
pc2_loadings <- fpca_m$rotation$data[2,]
pc3_loadings <- fpca_m$rotation$data[3,]
pc4_loadings <- fpca_m$rotation$data[4,]
pc5_loadings <- fpca_m$rotation$data[5,]


pc_scores1 <- fpca_m$x[,1]
pc_scores2 <- fpca_m$x[,2]
pc_scores3 <- fpca_m$x[,3]
pc_scores4 <- fpca_m$x[,4]
pc_scores5 <- fpca_m$x[,5]


subject_scores1 <- pc1_loadings %*% t(pc_scores1)
subject_scores2 <- pc2_loadings %*% t(pc_scores2)
subject_scores3 <- pc3_loadings %*% t(pc_scores3)
subject_scores4 <- pc4_loadings %*% t(pc_scores4)
subject_scores5 <- pc5_loadings %*% t(pc_scores5)


subject_scores12 <- subject_scores1 + subject_scores2
subject_scores13 <- subject_scores12 + subject_scores3
subject_scores14 <- subject_scores13 + subject_scores4
subject_scores15 <- subject_scores14 + subject_scores5



dim(subject_scores12)




# ----------
time <- time(subject_scores1) / 10



# darkgray:  original smoothed
# black:  only PC1, only PC1 + PC2
# blue:  sum of PC1 to PC5


par(mfrow = c(2, 2), mar = c(2,2,2,2))


idx <- 19

plot(ftensionNP$fdata.est[c(idx)], main = "middle: 19", col = "darkgray", lty = 2, lwd = 2)
lines(time, subject_scores1[,idx], lty = 2, lwd = 2, col = "black")
lines(time, subject_scores12[,idx], lty = 1, lwd = 2, col = "black")
lines(time, subject_scores15[,idx], lty = 1, lwd = 2, col = "blue")


idx <- 14

plot(ftensionNP$fdata.est[c(idx)], main = "positive: 14", col = "darkgray", lty = 2, lwd = 2)
lines(time, subject_scores1[,idx], lty = 2, lwd = 2, col = "black")
lines(time, subject_scores12[,idx], lty = 1, lwd = 2, col = "black")
lines(time, subject_scores15[,idx], lty = 1, lwd = 2, col = "blue")


idx <- 18

plot(ftensionNP$fdata.est[c(idx)], main = "positive: 18", col = "darkgray", lty = 2, lwd = 2)
lines(time, subject_scores1[,idx], lty = 2, lwd = 2, col = "black")
lines(time, subject_scores12[,idx], lty = 1, lwd = 2, col = "black")
lines(time, subject_scores15[,idx], lty = 1, lwd = 2, col = "blue")


idx <- 10

plot(ftensionNP$fdata.est[c(idx)], main = "negative: 10", col = "darkgray", lty = 2, lwd = 2)
lines(time, subject_scores1[,idx], lty = 2, lwd = 2, col = "black")
lines(time, subject_scores12[,idx], lty = 1, lwd = 2, col = "black")
lines(time, subject_scores15[,idx], lty = 1, lwd = 2, col = "blue")


idx <- 17

# scores for first component is alomost 0
plot(ftensionNP$fdata.est[c(idx)], main = "positive - negative: 17", col = "darkgray", lty = 2, lwd = 2)
lines(time, subject_scores1[,idx], lty = 2, lwd = 2, col = "black")
lines(time, subject_scores12[,idx], lty = 1, lwd = 2, col = "black")
lines(time, subject_scores15[,idx], lty = 1, lwd = 2, col = "blue")
