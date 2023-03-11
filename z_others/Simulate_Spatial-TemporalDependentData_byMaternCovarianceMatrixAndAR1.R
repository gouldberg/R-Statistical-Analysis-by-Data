# ------------------------------------------------------------------------------
# Simulate spatial and temporal dependency data
#
# spatial random effects by Matern correlation covariance matrix
# temporal dependency for each site by AR1
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# Generate sample spatial positions
# ------------------------------------------------------------------------------
sample_n <- 10

set.seed(123)
Xloc <- runif(sample_n, 0, 1)
Yloc <- runif(sample_n, 0, 1)
( Loc  <- cbind(Xloc, Yloc) )


# Check which pair of sites are close to each other
par(mfrow = c(1,1), cex.lab = 1.5, mar = c(5,5,2,2), cex.main = 1.5)
plot(x = Loc[,1], y = Loc[,2], type = "n", xlab = "X coordinate", ylab = "Y coordinate")
text(x = Loc[,1], y = Loc[,2], 1:sample_n, cex = 2)


# ------------------------------------------------------------------------------
# Calculate Matern correlation values: Cor.M
# ------------------------------------------------------------------------------
# Calculate the distance between each site
( Dist <- as.matrix(dist(Loc)) )


# Choose Matern correlation-specific parameter values arbitrarily
# kappa:  determines the distance at which dependency diminishes
# kappa is very much like AR1 correlation parameter phi for the time-series correlation of the range parameter in the exponential variogram model
kappa <- 5


# Calculate Matern correlation values --> Cor.M
d.vec <- as.vector(Dist)
Cor.M <- (kappa * d.vec)*besselK(kappa*d.vec, 1) 
Cor.M[1] <- 1
Cor.M


# plot the Matern correlation values versus distance
I <- order(d.vec)
plot(x = sort(d.vec), y = Cor.M[I], pch = 16, cex.lab = 1.5, xlab = "Distance",  ylab = "Correlation", type = "l")


# Cor.M with varying kappa
kappa <- 0.1
d.vec <- seq(0, 100, length = 100)      
Cor.M <- (kappa * d.vec) * besselK(kappa * d.vec, 1) 
Cor.M[1] <- 1
par(mfrow=c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
plot(x = d.vec, y = Cor.M, pch = 16, cex.lab = 1.5,xlab = "Distance", ylab = "Correlation", type = "n")

MyKappa <- c(0.02, 0.07, 0.3, 0.5, 0.9)
for (kappa in MyKappa){
  d.vec <- seq(0, 100, length = 100)      
  Cor.M <- (kappa * d.vec) * besselK(kappa * d.vec, 1) 
  Cor.M[1] <- 1
  lines(d.vec, Cor.M)
}     


# ------------------------------------------------------------------------------
# Calculate Covariance Matrix of the spatial random effects
#
# OMEGA: Sigma_u * Matern correlation sites i and j
# u ~ N(0, OMEGA): spatially correlated random effects
# The closer in space, the larger the values of the covariance matrix OMEGA and the more dependent the u valeues 
# ------------------------------------------------------------------------------
# Choose arbitrarily
kappa <- 5
Sigma_u <- 1

OMEGA <- matrix(nrow = sample_n, ncol = sample_n)

for (i in 1:sample_n){
  for (j in 1:sample_n){
    Distance <- Dist[i,j]
    Ch <- (kappa * Distance) * besselK(kappa * Distance, 1) 
    OMEGA[i,j] <- Sigma_u^2 * Ch
  }}
diag(OMEGA) <- 1

OMEGA


# ------------------------------------------------------------------------------
# Add also temporal dependency
# w[i,t]:  latent variable that changes in space and time = phi * w[i,t-1] + ui(t)
# i:  site   t:  time
# ------------------------------------------------------------------------------
# AR1 correlation parameter for temporal dependency
# small:  weak dependency   large:  strong dependency
phi <- 0.9

# Choose arbitrarily
Sigma_u  <- 1


library(MASS)
set.seed(1234)

w <- matrix(nrow = sample_n, ncol = 100)
Zeros <- rep(0, sample_n)

# set w for time = 1
Cov.w1 <-(Sigma_u^2 / (1-phi^2)) * OMEGA 
w.1 <- mvrnorm(1, mu = Zeros, Sigma = Cov.w1)
w[,1] <- w.1


# simulate temporal-dependent time series w[,t]
# w[,t]:  multiplying with the previous w values and adding some spatially correlated noise
for (t in 2:100){
  u <- mvrnorm(1, mu = Zeros, Sigma=OMEGA)
  w[,t] <- phi * w[,t-1] + u 
}

w


# ------------------------------------------------------------------------------
# Check to see that the site that are close in space behave similarly due to spacial correlation
# ------------------------------------------------------------------------------
# plot time series w
par(mfrow = c(1,1), mar= c(5,5,2,2), cex.lab = 1.5)
plot(x = 1:100, y = w[1,], type = "l", ylim = c(min(w),max(w)), xlab = "Time", ylab = "w", col = "gray")

# Choose spatially close points
i <- 7;  j <- 9;
lines(x = 1:100, y = w[i,], lwd = 1, lty = 1, col = "blue")
lines(x = 1:100, y = w[j,], lwd = 1, lty = 1, col = "red")


# Choose spatially NOT-close points
i <- 5;  j <- 6;
par(mfrow = c(1,1), mar= c(5,5,2,2), cex.lab = 1.5)
plot(x = 1:100, y = w[1,], type = "l", ylim = c(min(w),max(w)), xlab = "Time", ylab = "w", col = "gray")
lines(x = 1:100, y = w[i,], lwd = 1, lty = 1, col = "blue")
lines(x = 1:100, y = w[j,], lwd = 1, lty = 1, col = "red")

