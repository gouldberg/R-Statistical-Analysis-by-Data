
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\05_anomaly_detection\\02_異常検知_入力出力データ\\meatspec")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# data:  meatspec
#   - A Tecator Infratec Food and Feed Analyzer working in the wavelength range of 850 to 1050 nm
#     by the near-infrared transmittion (NIT) principle was used to collect data on samples of finely chopped pure
#     meat and 215 samples were measured.
#   - Fat content was measured along with a 101-channel spectrum of absorbances
# ------------------------------------------------------------------------------


# data(meatspec, package="faraway")

meatspec <- read.csv("meatspec.txt", header = T, sep = "\t")


names(meatspec)


# 100-channel spectrum of absorbances for 215 meat samples
dim(meatspec)




# ------------------------------------------------------------------------------
# data exploration
# ------------------------------------------------------------------------------


matplot(t(meatspec[,1:100]), type = "l", main = "X: 100-channel  Y: 215 meat samples' fat")




# ------------------------------------------------------------------------------
# data exploration:  pairwise high correlated variables
# ------------------------------------------------------------------------------

# find variables with pairwise high correlation (absolute value)

corThresh <- 0.9


tooHigh <- caret::findCorrelation(cor(meatspec), corThresh)


names(meatspec)[tooHigh]



# -->
# Many variables are correlated .... difficult for normal regression




# ------------------------------------------------------------------------------
# Select variables and transform data
# ------------------------------------------------------------------------------


# select all variables

cc <- paste0("V", 1:100)


Xc <- scale(meatspec[,cc])


psych::describe(Xc)


head(Xc)


Xc <- t(Xc)



# ------------------------------------------------------------------------------
# Eigen Value Decomposition and select m
# ------------------------------------------------------------------------------

S <- Xc %*% t(Xc)

evd <- eigen(S)



# ----------
plot(evd$values, type = "b", xlab = "index", ylab = "eigenvalue")


round(cumsum(evd$values) / sum(evd$values), 3)


# -->
# m = 1  (98.6% !!!)




# ------------------------------------------------------------------------------
# Compute normal space score
# ------------------------------------------------------------------------------


m <- 2


# scores for normal space:  2 scores for each car
( x2 <- t(evd$vectors[,1:m]) %*% Xc )


dim(x2)




# ----------
graphics.off()

par(mfrow = c(1,1))

plot(t(x2), xlab = "1st PCA", ylab = "2nd PCA")
text(t(x2)[,1]+0.1, t(x2)[,2]+0.1, colnames(x2), cex = 0.6)




# ------------------------------------------------------------------------------
# Compute anomaly scores
# ------------------------------------------------------------------------------

# anomaly scores = reconstructed errors = residuals ^ 2
a1 <- colSums(Xc * Xc) - colSums(x2 * x2)

a0 <- t(Xc) - rowSums(t(x2))




# ----------
# top 11
( idx <- order(a1, decreasing = T)[1:11] )


print(a1[idx])


a0[idx,]





# ----------
colorcode <- rep(0, ncol(x2))

colorcode[idx] <- 1 



# note that this is NORMAL SPACE REPRESENTATION !!!
graphics.off()

par(mfrow = c(1,1))

plot(t(x2)[,1], t(x2)[,2], xlab = "1st PCA", ylab = "2nd PCA", pch = 21, bg = colorcode, cex = 1.2)
text(t(x2)[,1]+0.1, t(x2)[,2]+0.1, colnames(x2), cex = 0.6)


a0[idx]




# ----------
matplot(t(meatspec[idx,1:100]), type = "l", main = "X: 100-channel  Y: anomaly meat samples' fat", 
        lty = 1, lwd = 2)




# ------------------------------------------------------------------------------
# Compute Anomaly Scores, computing from Gramm Matrix (if nrow(data) < ncol(data))
# ------------------------------------------------------------------------------


G <- t(Xc) %*% Xc


evd <- eigen(G)


Lam_12 <- diag(evd$values[1:m] ^ (-0.5))


xx2 <- Lam_12 %*% t(evd$vectors[,1:m]) %*% t(Xc) %*% Xc


# anomaly scores
aa1 <- colSums(Xc * Xc) - colSums(xx2 * xx2)


( idx <- order(aa1, decreasing = T)[1:6] )


print(aa1[idx])




