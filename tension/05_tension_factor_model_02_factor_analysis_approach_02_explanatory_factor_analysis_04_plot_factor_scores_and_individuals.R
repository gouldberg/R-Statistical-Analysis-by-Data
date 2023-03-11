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
# plot factor scores + individual time series
# ------------------------------------------------------------------------------

# we apply unorthogonal solutions (resFA3)


# 29 * 3
head(resFA3$weights)


# 29 * 3
resFA3$loadings


# 800 * 3
resFA3_scores <- factor.scores(tens, f = resFA3, method = "Thurstone")$scores

head(resFA3_scores)



# ----------
# individuals scores

( pred_f <- resFA3_scores %*% t(resFA3$weights) )


res_scores_f <- data.frame(resFA3_scores)



# ----------
sort(round(resFA3$communality, 4))




# ----------
# comparison with PCA and functional analysis

graphics.off()

par(mfrow = c(3,1), mar = c(2,2,2,2))


# factor analysis scores
plot(res_scores_f[,"ML1"], type = "l", lty = 1, col = "red", lwd = 2, ylim = c(-3, 3), main = "ordinary factor analysis scores", cex.main = 2)
abline(h = 0, lty = 2, col = "gray")

lines(res_scores_f[,"ML2"], type = "l", lty = 2, col = "coral", lwd = 2)
lines(res_scores_f[,"ML3"], type = "l", lty = 3, col = gray(0.7))


# PCA loadings
plot(res_scores[,"Comp.1"], type = "l", lty = 1, col = "red", lwd = 2, ylim = c(-8, 9), main = "ordinary PCA loadings", cex.main = 2)
abline(h = 0, lty = 2, col = "gray")

lines(res_scores[,"Comp.2"], type = "l", lty = 2, col = "coral", lwd = 2)
lines(res_scores[,"Comp.3"], type = "l", lty = 3, col = gray(0.7))


# Functional analysis loadings
plot(fpca$rotation[1,]$argvals, fpca$rotation$data[1,], type = "l", 
     ylim = c(-0.3, 0.3), xlab = "Time",  ylab = "Loadings", main = "Functional PCA Loadings",
     lwd = 2, col = "red", cex.main = 2)
abline(h = 0, lty = 2, col = "gray")
lines(fpca$rotation$argvals, fpca$rotation$data[2,], type = "l", col = "coral", lwd = 2, lty = 2)
legend("topright", legend = c("PC1", "PC2"), lty = c(1,2), col = c("red", "coral"), cex = 1.2)




# ----------
# comparison with functional analysis

dimnames(tens)[[2]] <- colnames(tens)



par(mfcol = c(3, 2), mar = c(2,2,2,2))


idx <- 19

plot(tens[,idx], type = "l", col = "darkgray", lty = 2, lwd = 2, ylim = c(-3, 2))
lines(pred_f[,idx], type = "l", lty = 1, lwd = 2, col = "blue")

plot(tens[,idx], type = "l", col = "darkgray", lty = 2, lwd = 2, ylim = c(-3, 2))
lines(pred[,idx], type = "l", lty = 1, lwd = 2, col = "blue")


plot(ftensionNP$fdata.est[c(idx)], main = "middle: 19", col = "darkgray", lty = 2, lwd = 2)
lines(time, subject_scores1[,idx], lty = 2, lwd = 2, col = "black")
lines(time, subject_scores12[,idx], lty = 1, lwd = 2, col = "black")
lines(time, subject_scores15[,idx], lty = 1, lwd = 2, col = "blue")


idx <- 14

plot(tens[,idx], type = "l", col = "darkgray", lty = 2, lwd = 2, ylim = c(-3, 2))
lines(pred_f[,idx], type = "l", lty = 1, lwd = 2, col = "blue")

plot(tens[,idx], type = "l", col = "darkgray", lty = 2, lwd = 2, ylim = c(-3, 2))
lines(pred[,idx], type = "l", lty = 1, lwd = 2, col = "blue")

plot(ftensionNP$fdata.est[c(idx)], main = "positive: 14", col = "darkgray", lty = 2, lwd = 2)
lines(time, subject_scores1[,idx], lty = 2, lwd = 2, col = "black")
lines(time, subject_scores12[,idx], lty = 1, lwd = 2, col = "black")
lines(time, subject_scores15[,idx], lty = 1, lwd = 2, col = "blue")


idx <- 18

plot(tens[,idx], type = "l", col = "darkgray", lty = 2, lwd = 2, ylim = c(-3, 2))
lines(pred_f[,idx], type = "l", lty = 1, lwd = 2, col = "blue")

plot(tens[,idx], type = "l", col = "darkgray", lty = 2, lwd = 2, ylim = c(-3, 2))
lines(pred[,idx], type = "l", lty = 1, lwd = 2, col = "blue")

plot(ftensionNP$fdata.est[c(idx)], main = "positive: 18", col = "darkgray", lty = 2, lwd = 2)
lines(time, subject_scores1[,idx], lty = 2, lwd = 2, col = "black")
lines(time, subject_scores12[,idx], lty = 1, lwd = 2, col = "black")
lines(time, subject_scores15[,idx], lty = 1, lwd = 2, col = "blue")


idx <- 10

plot(tens[,idx], type = "l", col = "darkgray", lty = 2, lwd = 2, ylim = c(-3, 2))
lines(pred_f[,idx], type = "l", lty = 1, lwd = 2, col = "blue")

plot(tens[,idx], type = "l", col = "darkgray", lty = 2, lwd = 2, ylim = c(-3, 2))
lines(pred[,idx], type = "l", lty = 1, lwd = 2, col = "blue")

plot(ftensionNP$fdata.est[c(idx)], main = "negative: 10", col = "darkgray", lty = 2, lwd = 2)
lines(time, subject_scores1[,idx], lty = 2, lwd = 2, col = "black")
lines(time, subject_scores12[,idx], lty = 1, lwd = 2, col = "black")
lines(time, subject_scores15[,idx], lty = 1, lwd = 2, col = "blue")


idx <- 17

plot(tens[,idx], type = "l", col = "darkgray", lty = 2, lwd = 2, ylim = c(-3, 2))
lines(pred_f[,idx], type = "l", lty = 1, lwd = 2, col = "blue")

plot(tens[,idx], type = "l", col = "darkgray", lty = 2, lwd = 2, ylim = c(-3, 2))
lines(pred[,idx], type = "l", lty = 1, lwd = 2, col = "blue")

# scores for first component is alomost 0
plot(ftensionNP$fdata.est[c(idx)], main = "positive - negative: 17", col = "darkgray", lty = 2, lwd = 2)
lines(time, subject_scores1[,idx], lty = 2, lwd = 2, col = "black")
lines(time, subject_scores12[,idx], lty = 1, lwd = 2, col = "black")
lines(time, subject_scores15[,idx], lty = 1, lwd = 2, col = "blue")
