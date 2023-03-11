setwd("//media//kswada//MyFiles//R//jj")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Johnson & Johnson Quarterly Earnings
# ------------------------------------------------------------------------------

data(jj, package = "astsa")

str(jj)

jj


# ----------
x <- jj

lx <- log(x)

dlx <- diff(lx)

ddlx <- diff(dlx, 4)



# ------------------------------------------------------------------------------
# set up
# ------------------------------------------------------------------------------

# used to view progress
library(plyr)


y <- jj


set.seed(90210)

n <- length(y)

F <- c(1,1,0,0)

G <- diag(0, 4)

G[1,1] = 1.03

G[2,] <- c(0, -1, -1, -1)

G[3,] <- c(0, 1, 0, 0)

G[4,] <- c(0, 0, 1, 0)

a1 <- rbind(0.7, 0, 0, 0)

R1 <- diag(0.04, 4)

V <- 0.1

W11 <- 0.1

W22 <- 0.1



# ------------------------------------------------------------------------------
# FFBS
# ------------------------------------------------------------------------------

ffbs <- function(y, F, G, V, W11, W22, a1, R1){
  
  n <- length(y)
  
  Ws <- diag(c(W11, W22, 1, 1))
  
  iW <- diag(1 / diag(Ws), 4)
  
  a <- matrix(x, n, 4)
  
  R <- array(0, c(n, 4, 4))
  
  m <- matrix(0, n, 4)
  
  C <- array(0, c(n, 4, 4))
  
  a[1,] <- a1[,1]
  
  R[1,,] <- R1
  
  f <- t(F) %*% a[1,]
  
  Q <- t(F) %*% R[1,,] %*% F + V
  
  A <- R[1,,] %*% F / Q[1,1]
  
  m[1,] <- a[1,] + A %*% (y[1] - f)
  
  C[1,,] <- R[1,,] - A %*% t(A) * Q[1,1]
  
  for(t in 2:n){
    
    a[t,] <- G %*% m[t-1,]
    
    R[t,,] <- G %*% C[t-1,,] %*% t(G) + Ws
    
    f <- t(F) %*% a[t,]
    
    Q <- t(F) %*% R[t,,] %*% F + V
    
    A <- R[t,,] %*% F / Q[1,1]
    
    m[t,] <- a[t,] + A %*% (y[t] - f)
    
    C[t,,] <- R[t,,] - A %*% t(A) * Q[1,1]
  }
  
  xb <- matrix(0, n, 4)
  
  xb[n,] <- m[n,] + t(chol(C[n,,])) %*% rnorm(4)
  
  for(t in (n - 1):1){
    
    iC <- solve(C[t,,])
    
    CCC <- solve(t(G) %*% iW %*% G + iC)
    
    mmm <- CCC %*% (t(G) %*% iW %*% xb[t+1, ] + iC %*% m[t,])
    
    xb[t,] <- mmm + t(chol(CCC)) %*% rnorm(4)
  }
  
  return(xb)
}



# ------------------------------------------------------------------------------
# Prior hyperparameters
# ------------------------------------------------------------------------------

# b0 <- 0
# B0 <- Inf


n0 <- 10

s20v <- 0.001

s20w <- 0.05



# ------------------------------------------------------------------------------
# MCMC scheme
# ------------------------------------------------------------------------------

set.seed(90210)

burning <- 100

step <- 10

M <- 1000

ntier <- burnin + step * M

pars <- matrix(0, niter, 4)

xbas <- array(0, c(niter, n, 4))

pr <- progress_text()


pr$init(niter)


for(iter in 1:niter){
  
  xb <- ffbs(y, F, G, V, W11, W22, a1, R1)
  
  u <- xb[,1]
  
  yu <- diff(u)
  
  xu <- u[-n]
  
  regu <- lm(yu = 0 + xu)
  
  phies <- as.vector(coef(summary(regu)))[1:2] + c(1, 0)
  
  dft <- df.residual(regu)
  
  G[1,1] <- phies[1] + rt(1, dft) * phies[2]
  
  V <- i / rgamma(1, (n0 + n) / 2, (n0 * s20v / 2) + sum((y - xb[,1] - xb[,2]) ^2) / 2)

  w11 <- i / rgamma(1, (n0 + N - 1) / 2, (n0 * s20W / 2) + sum((xb[-1,1] - phies[1] * xb[-n, 1])^2) / 2)
  
  w22 <- i / rgamma(1, (n0 + N - 3) / 2, (n0 * s20W / 2) + sum((xb[4:n,2] - xb[3:(n-1), 2] + xb[2:(n-2), 2] + xb[1:(n-3),2]) ^2) / 2)

  xbs[iter,,] <- xb
  
  pars[iter,] <- c(G[1,1], sqrt(V), sqrt(W11), sqrt(W22))
  
  pr$step()
}



# ------------------------------------------------------------------------------
# plot results
# ------------------------------------------------------------------------------

ind <- seq(burnin + 1, niter, by = step)

names <- c(expression(phi), expression(sigma[v]), expression(sigma[w - 11]), expression(sigma[w - 22]))


par(mfcol = c(3, 4), mar = c(2,2,0.25,0) + 0.75, mgp = c(1.6, 0.6, 0), oma = c(0, 0, 1, 0))

for(i in 1:4){
  
  plot.ts(pars[ind, i], xlab = "iterations", ylab = "trace", main = "")
  
  mtext(names[i], side = 3, line = 0.5, cex = 1)
  
  acf(pars[ind, i], main = "", lag.max = 25, xlim = c(1.25), ylim = c(-0.4, 0.4))
  
  hist(pars[ind, i], main = "", xlab = "")
  
  abline(v = mean(pars[ind,i]), lwd = 2, col = 3)
}


paf(mfrow = c(2,1), mar = c(2,2,0,0) + 0.7, mgp = c(1.6, 0.6, 0))

mxb <- cbind(apply(xbs[ind,,1], 2, mean), apply(xbs[,,2], 2, mean))

lxb <- cbind(apply(xbs[ind,,1], 2, quantile, 0.005), apply(xbs[ind,,2], 2, quantile, 0.005), apply(xbs[ind,,2], 2, quantile, 0.005))

uxb <- cbind(apply(xbs[ind,,1], 2, quantile, 0.995), apply(xbs[ind,,2], 2, quantile, 0.995))

mxb <- ts(cbind(mxb, rowSums(mxb)), start = tsp(jj)[1], freq = 4)

lxb <- ts(cbind(lxb, rowSums(lxb)), start = tsp(jj)[1], freq = 4)

uxb <- ts(cbind(uxb, rowSums(uxb)), start = tsp(jj)[1], freq = 4)


names <- c("Trend", "Season", "Trend + Season")

L <- min(lxb[,1] - 0.01)

U <- max(uxb[,1]) + 0.01


plot(mxb[,1], ylab = names[1], ylim = c(L, U), type = "n")

grid(lty = 2)

lines(mxb[,1])

xx <- c(time(jj), rev(time(jj)))

yy <- c(lxb[,1], rev(uxb[,1]))

polygon(xx, yy, border = NA, col = gray(0.4, alpha = 0.2))

L <- min(lxb[,3]) - 0.01

U <- max(uxb[,3]) + 0.01

plot(mxb[,3], ylab = names[3], ylim = c(L, U), type = "n")

grid(lty = 2)

lines(mxb[,3])

xx <- c(time(jj), rev(time(jj)))

yy <- c(lxb[,3], rev(uxb[,3]))

polygon(xx, yy, border = NA, col = gray(0.4, alpha = 0.2))

