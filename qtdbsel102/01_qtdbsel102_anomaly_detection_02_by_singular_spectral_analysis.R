rm(list = ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\05_anomaly_detection\\qtdbsel102")



# ------------------------------------------------------------------------------
# data:  qtdbsel102
# ------------------------------------------------------------------------------


data <- read.csv(file = "qtdbsel102.txt", header = FALSE, sep = "")



str(data)


head(data)





# ------------------------------------------------------------------------------
# Change Point Detection by Singular Spectral Analysis
# ------------------------------------------------------------------------------


xi <- data[3001:6000, 2]



# ----------
# try 50 and 25

w <- 50
# w <- 25


m <- 2


k <- w/2


L <- k/2


Tt <- length(xi)




# ----------
score <- rep(0, Tt)


for(t in (w + k):(Tt - L + 1)){
  
  # -----------
  # left matrix
  tstart <- t - w - k + 1
  
  tend <- t - 1
  
  X1 <- t(embed(xi[tstart:tend], w))
  
  
  # reverse time
  X1 <- X1[w:1,]
  
  

  # -----------
  # right matrix
  tstart <- t - w - k + 1
  
  tend <- t - 1 + L
  
  X2 <- t(embed(xi[tstart:tend], w))
  
  
  X2 <- X2[w:1,]
  
  
  
  # ----------
  U1 <- svd(X1)$u[,1:m]
  
  U2 <- svd(X2)$u[,1:m]
  
  
  # degree of overwrapping of subspaces
  sig1 <- svd(t(U1) %*% U2)$d[1]
  
  
  # scores
  score[t] <- 1 - sig1 ^ 2
}




# ----------

graphics.off()

par(mfrow = c(2,1))

rng <- c(1260, 1340)

plot(xi, type = "l", main = "original series", ylim = c(4, 6))
abline(v = rng, lty = 2, col = "red")

plot(score, type = "l", lty = 1, col = "blue", main = "degree of change")
abline(v = rng, lty = 2, col = "red")





# ------------------------------------------------------------------------------
# Change Point Detection by Singular Spectral Analysis for V3
# ------------------------------------------------------------------------------


xi <- data[3001:6000, 3]



# ----------
# try 50 and 25

w <- 50
# w <- 25


m <- 2


k <- w/2


L <- k/2


Tt <- length(xi)




# ----------
score <- rep(0, Tt)


for(t in (w + k):(Tt - L + 1)){
  
  # -----------
  # left matrix
  tstart <- t - w - k + 1
  
  tend <- t - 1
  
  X1 <- t(embed(xi[tstart:tend], w))
  
  
  # reverse time
  X1 <- X1[w:1,]
  
  
  
  # -----------
  # right matrix
  tstart <- t - w - k + 1
  
  tend <- t - 1 + L
  
  X2 <- t(embed(xi[tstart:tend], w))
  
  
  X2 <- X2[w:1,]
  
  
  
  # ----------
  U1 <- svd(X1)$u[,1:m]
  
  U2 <- svd(X2)$u[,1:m]
  
  
  # degree of overwrapping of subspaces
  sig1 <- svd(t(U1) %*% U2)$d[1]
  
  
  # scores
  score[t] <- 1 - sig1 ^ 2
}




# ----------

graphics.off()

par(mfrow = c(2,1))

rng <- c(1260, 1340)

plot(xi, type = "l", main = "original series", ylim = c(0, 10))
abline(v = rng, lty = 2, col = "red")

plot(score, type = "l", lty = 1, col = "blue", main = "degree of change")
abline(v = rng, lty = 2, col = "red")


