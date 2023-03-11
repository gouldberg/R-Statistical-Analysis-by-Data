
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\05_anomaly_detection\\02_異常検知_入力出力データ\\meatspec")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# data:  meatspec
# ------------------------------------------------------------------------------


# data(meatspec, package="faraway")

meatspec <- read.csv("meatspec.txt", header = T, sep = "\t")


names(meatspec)


# 100-channel spectrum of absorbances for 215 meat samples
dim(meatspec)




# ------------------------------------------------------------------------------
# prediction by ridge regression
# ------------------------------------------------------------------------------


# select all variables

cc <- paste0("V", 1:100)



# need scaling
Xc <- scale(meatspec[,cc])


psych::describe(Xc)


head(Xc)




# y:  fat
y <- meatspec[,"fat"]


M <- ncol(Xc)

N <- length(y)




# ----------
# set lambda and ridge regression selecting best lambda by cross validation

lambdas <- seq(-10, 10, length = 20000)


model <- lm.ridge(y ~ ., data.frame(cbind(Xc, y)), lambda = lambdas)



( bestIdx <- which.min(model$GCV) )


# selected lambda
lam <- model$lambda[bestIdx]



# ----------
# prediction

( coefs <- coef(model)[bestIdx,] )


ypred <- as.matrix(Xc) %*% as.matrix(coefs[2:101]) + coefs[1]





# ------------------------------------------------------------------------------
# Compute anomaly scores
# ------------------------------------------------------------------------------

sig2 <- (lam * sum(coefs[2:101] ^ 2) + sum(as.numeric(ypred) - y)^2) / N


X_ <- t(scale(Xc, scale = F))


H <- t(X_) %*% solve(X_ %*% t(X_) + lam * diag(M), X_)


TrHN <- sum(diag(H)) / N


# anomaly scores
a <- (as.numeric(ypred) - y) ^ 2 / ((1 - TrHN) ^ 2 * sig2)




# ----------
graphics.off()
par(mfrow = c(1,2))


plot(ypred ~ y, xlab = "original", ylab = "pred")
abline(0, 1)

plot(a, xlab = "index", ylab = "anomaly score")

th <- sort(a)[N * (1 - 0.05)]

lines(0:215, rep(th, length(0:215)), col = "red", lty = 2)




# ----------
graphics.off()

par(mfrow = c(1,1))

( idx <- which(a > th) )


matplot(t(meatspec[idx,1:100]), type = "l", main = "X: 100-channel  Y: anomaly meat samples' fat", 
        lty = 1, lwd = 2)


