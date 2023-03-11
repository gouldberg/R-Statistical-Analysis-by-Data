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



# ------------------------------------------------------------------------------
# standardize individual series
# ------------------------------------------------------------------------------


std <- diag(1 / sqrt(diag(cov(tens))))


tens <- as.matrix(tens) %*% std




# ------------------------------------------------------------------------------
# plot PC scores + individual time series
# ------------------------------------------------------------------------------


# 29(subject) * 29(Comp)
m1$loadings



# 800 * 10(Comp)
m1$scores



# ----------
# individuals scores:  1 - 5 components


( pred <- data.frame(m1$scores[,1:5] %*% t(m1$loadings[,1:5])) )

colnames(pred) <- colnames(tens) <- paste0("X", 1:29)


res_scores <- data.frame(m1$scores)





# ----------
# comparison with functional analysis

graphics.off()

par(mfrow = c(2,1), mar = c(2,2,2,2))


# factor scores
plot(res_scores[,"Comp.1"], type = "l", lty = 1, col = "red", lwd = 2, ylim = c(-8, 9), main = "ordinary PCA loadings", cex.main = 2)
abline(h = 0, lty = 2, col = "gray")

lines(res_scores[,"Comp.2"], type = "l", lty = 2, col = "coral", lwd = 2)
lines(res_scores[,"Comp.3"], type = "l", lty = 3, col = gray(0.7))


plot(fpca$rotation[1,]$argvals, fpca$rotation$data[1,], type = "l", 
     ylim = c(-0.3, 0.3), xlab = "Time",  ylab = "Loadings", main = "Functional PCA Loadings",
     lwd = 2, col = "red", cex.main = 2)
abline(h = 0, lty = 2, col = "gray")
lines(fpca$rotation$argvals, fpca$rotation$data[2,], type = "l", col = "coral", lwd = 2, lty = 2)
legend("topright", legend = c("PC1", "PC2"), lty = c(1,2), col = c("red", "coral"), cex = 1.2)




# ----------
# comparison with functional analysis

dimnames(tens)[[2]] <- colnames(tens)



par(mfcol = c(2, 2), mar = c(2,2,2,2))


idx <- 19

plot(tens[,idx], type = "l", col = "darkgray", lty = 2, lwd = 2, ylim = c(-3, 2))
lines(pred[,idx], type = "l", lty = 1, lwd = 2, col = "blue")


plot(ftensionNP$fdata.est[c(idx)], main = "middle: 19", col = "darkgray", lty = 2, lwd = 2)
lines(time, subject_scores1[,idx], lty = 2, lwd = 2, col = "black")
lines(time, subject_scores12[,idx], lty = 1, lwd = 2, col = "black")
lines(time, subject_scores15[,idx], lty = 1, lwd = 2, col = "blue")


idx <- 14

plot(tens[,idx], type = "l", col = "darkgray", lty = 2, lwd = 2, ylim = c(-3, 2))
lines(pred[,idx], type = "l", lty = 1, lwd = 2, col = "blue")

plot(ftensionNP$fdata.est[c(idx)], main = "positive: 14", col = "darkgray", lty = 2, lwd = 2)
lines(time, subject_scores1[,idx], lty = 2, lwd = 2, col = "black")
lines(time, subject_scores12[,idx], lty = 1, lwd = 2, col = "black")
lines(time, subject_scores15[,idx], lty = 1, lwd = 2, col = "blue")


idx <- 18

plot(tens[,idx], type = "l", col = "darkgray", lty = 2, lwd = 2, ylim = c(-3, 2))
lines(pred[,idx], type = "l", lty = 1, lwd = 2, col = "blue")

plot(ftensionNP$fdata.est[c(idx)], main = "positive: 18", col = "darkgray", lty = 2, lwd = 2)
lines(time, subject_scores1[,idx], lty = 2, lwd = 2, col = "black")
lines(time, subject_scores12[,idx], lty = 1, lwd = 2, col = "black")
lines(time, subject_scores15[,idx], lty = 1, lwd = 2, col = "blue")


idx <- 10

plot(tens[,idx], type = "l", col = "darkgray", lty = 2, lwd = 2, ylim = c(-3, 2))
lines(pred[,idx], type = "l", lty = 1, lwd = 2, col = "blue")

plot(ftensionNP$fdata.est[c(idx)], main = "negative: 10", col = "darkgray", lty = 2, lwd = 2)
lines(time, subject_scores1[,idx], lty = 2, lwd = 2, col = "black")
lines(time, subject_scores12[,idx], lty = 1, lwd = 2, col = "black")
lines(time, subject_scores15[,idx], lty = 1, lwd = 2, col = "blue")


idx <- 17

plot(tens[,idx], type = "l", col = "darkgray", lty = 2, lwd = 2, ylim = c(-3, 2))
lines(pred[,idx], type = "l", lty = 1, lwd = 2, col = "blue")

# scores for first component is alomost 0
plot(ftensionNP$fdata.est[c(idx)], main = "positive - negative: 17", col = "darkgray", lty = 2, lwd = 2)
lines(time, subject_scores1[,idx], lty = 2, lwd = 2, col = "black")
lines(time, subject_scores12[,idx], lty = 1, lwd = 2, col = "black")
lines(time, subject_scores15[,idx], lty = 1, lwd = 2, col = "blue")
