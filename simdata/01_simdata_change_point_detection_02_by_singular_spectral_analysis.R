rm(list = ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\05_anomaly_detection\\simdata")



# ------------------------------------------------------------------------------
# data:  generation
# ------------------------------------------------------------------------------


set.seed(1)


tt <- 0.1


x1 <- seq(0, 10, by = tt)


x2 <- seq(10.1, 20, by = tt)


x3 <- seq(20.2, 30, by = tt)


y1 <- sin(pi * x1) + rnorm(length(x1), sd = 0.07)


y2 <- sin(2 * pi * x2) + rnorm(length(x2), sd = 0.07)


y3 <- sin(pi * x3) + rnorm(length(x3), sd = 0.07)


xi <- c(y1, y2, y3)




# ----------
par(mfrow = c(1,1))

plot(xi, type = "l")




# ------------------------------------------------------------------------------
# Change Point Detection by Singular Spectral Analysis
# ------------------------------------------------------------------------------


w <- 10


m <- 2


k <- 10


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

rng <- c(100, 107, 115, 202, 208, 215)

plot(xi, type = "l", main = "original series")
abline(v = rng, lty = 2, col = "red")

plot(score, type = "l", lty = 1, col = "blue", main = "degree of change")
abline(v = rng, lty = 2, col = "red")


