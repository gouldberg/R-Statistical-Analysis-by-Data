# ------------------------------------------------------------------------------
# Simulate Gamma GLM and ZAG (Zero-Altered Gamma) Model
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

rm(list=ls())


# ------------------------------------------------------------------------------
# Simulating gammma distributed data
# ------------------------------------------------------------------------------
graphics.off()
par(mar = c(5,5,2,2), cex.lab = 1.5, mfrow = c(3,3))

# Generating gamma distributed data
set.seed(123) 
N     <- 1000
beta1 <- 1
beta2 <- 2
scale <- 1
X1    <- sort(runif(N))
( mu    <- exp(beta1 + beta2 * X1) )
Y     <- rgamma(n = N, shape = mu, scale = scale)


# Fitted values from teh gamma GLM
G1 <- glm(Y  ~ X1, family = Gamma(link = "log") )
summary(G1)
# NOTE that a gamma GLM cannot be overdispersed
summary(G1)$dispersion

length <- 50
MyData <- data.frame(X1 = seq(0, 1, length = length))
X <- model.matrix(~ X1, data = MyData)
Beta <- coef(G1)
fit  <- exp(X %*% Beta)

( Shape <- 1 / summary(G1)$dispersion )

ylim <- c(0, ceiling(max(Y)))
plot(x = X1, y = Y, ylim = ylim, xlab = "Covariate", ylab = "Response", cex = 0.5)
lines(x = MyData$X1, y = fit, lwd = 6, col = "blue")


# residuals
E <- resid(G1, type = "pearson")
Er <- resid(G1, type = "response")
xlim <- c(0, ceiling(max(Y)))

# Response residuals versus Covariate
plot(x = X1, y = Er, xlab = "Covariate", ylab = "Response residuals", cex = 0.5)
abline(h = 0, lty = 2)     

# Response residuals versus Fitted Value
plot(x = fitted(G1), y = Er, xlim = xlim, xlab = "Fitted values", ylab = "Response residuals", cex = 0.5)
abline(h = 0, lty = 2)     

# Pearson Residuals versus Covariate
plot(x = X1, y = E, xlab = "Covariate", ylab = "Pearson residuals", cex = 0.5)
abline(h = 0, lty = 2)     

# Pearson Residuals versus Fitted Value
plot(x = fitted(G1), y = E, xlim = xlim, xlab = "Fitted values", ylab = "Pearson residuals", cex = 0.5)
abline(h = 0, lty = 2)     


# ------------------------------------------------------------------------------
# Visualizing the model fit in 3D:  Density curves
# ------------------------------------------------------------------------------
z1 <- MyData$X1
Z <- matrix(nrow = length, ncol=length)
for (i in 1:length){
  Scale <- fit[i] / Shape 
  Z[,i] <- dgamma(x=fit, shape=Shape, scale = Scale )
}

par(mfrow=c(1,1))
persp(x = z1, y = fit, z = Z,
      scale = TRUE,
      theta = 130, phi = 20, expand = 1,
      ltheta = 120, shade = 0.5, 
      ticktype = "detailed",
      xlab = "Covariate", 
      ylab = "Response", 
      zlab = "Probability",
      main = "")  -> res
round(res,3)
lines (trans3d(z1, y = fit, 
               z = rep(0,length), 
               pmat = res), col = 1, lwd=5)


# ------------------------------------------------------------------------------
# Visualizing the model fit with simulated gamma data
# ------------------------------------------------------------------------------
par(mfrow=c(1,1))

par(mar = c(5,5,2,2), cex.lab = 1.5)
plot(x = X1, y = Y, pch = 1, ylim = c(0,30), xlab = "Covariate", ylab = "Response")
lines(x = MyData$X1, y = fit, lwd = 5)

Myi <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)

sample_n <- 50

# vertical bands of points on top of the line are samples from a gamma distribution with the mean given by the line
for (i in 1:length(Myi)){
  fiti <- exp(Beta[1] + Myi[i] * Beta[2])
  fiti <- as.numeric(fiti)
  Scale <- fiti / Shape 
  yr <- rgamma(n = length, shape = fiti / Scale, scale = Scale )
  points(x = rep(Myi[i], sample_n), y = yr, pch = 16, col = grey(0.4))
}


# ------------------------------------------------------------------------------
# Simulating gammma distributed data
# "pscl" packages has no facilities for continuous zero-inflated data --> use glm function
# ------------------------------------------------------------------------------
set.seed(123)

# Simulate Bernoulli data 
N     <- 1000
gamma1  <- -1
eta     <- gamma1
( Pi      <- exp(eta) / (1 + exp(eta)) )
W       <- rbinom(N, size = 1, prob = Pi)
table(W)


# Simulate continuous data
beta1 <- 1
beta2 <- 2
scale <- 1
X1    <- sort(runif(N))
( mu    <- exp(beta1 + beta2 * X1) )
Yg    <- rgamma(n = N, shape = mu, scale = scale)


# And this is the ZAG data
Y <- W * Yg

DataHurdle <- data.frame(
  Y  = Y,
  X1 = X1,
  Yg = Yg,
  mu = mu,
  W  = W)

head(DataHurdle, 10)


# estimated parameters
DataHurdle.Pos <- subset(DataHurdle, Y > 0)
H1 <- glm(Y ~ X1, family = Gamma(link = "log"), data = DataHurdle.Pos)

DataHurdle$Y.01 <- as.numeric(DataHurdle$Y > 0)          
H2 <- glm(Y.01 ~ 1, family = binomial, data = DataHurdle)

summary(H1)
summary(H2)

beta  <- coef(H1)
gamma <- coef(H2)

# the glm function uses a quick and dirty approach to estimate the parameter r for a gamma GLM after the glm function finishes the parameter estimation process, namely via residuals
( r     <- 1 / summary(H1)$dispersion )

# !!! better approach to estiamte r is via gamma.shape function from the MASS package
library(MASS)
gamma.shape(H1)


# expected mean values of the gamma GLM
Xg <- model.matrix(H1)
mu.gamma <- exp(Xg %*% beta)
mu <- rep(1, N)
mu[W == 1] <- mu.gamma


# expected mean values of the Bernoulli GLM
Pi <- exp(gamma) / (1 + exp(gamma))
Pi <- as.numeric(Pi)


# mean and variance of ZAG model
ExpY <- Pi * mu
VarY <- (1 / r) * (Pi * r + Pi - Pi^2 * r ) * mu^2 


graphics.off()
par(mar = c(5,5,2,2), cex.lab = 1.5, mfrow = c(3,3))

# Simulated data versus Covariate X1
plot(x = DataHurdle$X1, y = DataHurdle$Y, xlab = "Covariate X1", ylab = "Simulated data", pch = 16, cex = 0.8)
lines(x = DataHurdle.Pos$X1, y = mu.gamma, lwd = 5, col="blue")
lines(x = DataHurdle.Pos$X1, y = Pi * mu[W == 1], lwd = 7, col="salmon")
abline(h = Pi, lwd = 3)

# Fitted values versus simulated data
plot(x = as.numeric(ExpY), y = DataHurdle$Y, xlim = xlim, xlab = "Fitted values", ylab = "Observed data")

# residuals
Er <- DataHurdle$Y - ExpY
E <- Er / sqrt(VarY)
xlim <- c(0, ceiling(max(ExpY)))

# Response residuals versus Covariate X1
plot(x = DataHurdle$X1, y = Er, xlab = "Covariate X1", ylab = "Response residuals")

# Response residuals versus Fitted Value
plot(x = ExpY, y = Er, xlim = xlim, xlab = "Fitted values", ylab = "Response residuals", cex = 0.5)

# Pearson residuals versus Covariate X1
plot(x = DataHurdle$X1, y = E, xlab = "Covariate X1", ylab = "Pearson residuals")

# Pearson residuals versus Fitted Value
plot(x = ExpY, y = E, xlim = xlim, xlab = "Fitted values", ylab = "Pearson residuals", cex = 0.5)


# ------------------------------------------------------------------------------
# Visualizing the ZAP model fit in 3D:  Density curves
# ------------------------------------------------------------------------------
library(scatterplot3d) 
par(mfrow=c(1,1))

# Three variable for 3-d scatterplot
x <- 0
y <- 0
z <- 0 * x
ylim <- c(0, max(Y))

rr <- scatterplot3d(x, y, z, 
                   highlight.3d = FALSE, 
                   col.axis = "black",
                   col.grid = "black", 
                   pch = 20,
                   angle = 40,
                   xlim = c(0, 1),
                   zlim = c(0, 0.8),
                   ylim = ylim,
                   type="l",
                   lwd = 3,
                   grid = FALSE,
                   box = TRUE,
                   cex.lab = 1.5,
                   xlab = "Covariate",
                   ylab = "Possible values",
                   zlab = "Probability")

# Simulated data
rdat <- cbind(DataHurdle$X1, DataHurdle$Y, rep(0,nrow(DataHurdle)))
rr$points3d(rdat, col = 1, type = "p", pch = 16, cex = 1)

# Fitted values per model
xx1      <- seq(0, 1, length = 50)
xmat     <- model.matrix(~1 + xx1)
fitgamma <- exp(xmat %*% beta)
fitzap   <- Pi * fitgamma

# ZAP model
rdat <- cbind(xx1, fitzap, rep(0, length=length))
rr$points3d(rdat, col = "salmon", type = "l", lwd = 7)

# Gamma model
rdat <- cbind(xx1, fitgamma, rep(0, length=length))
rr$points3d(rdat, col = "blue", type = "l", lwd = 4)

# Bernoulli model
rdat <- cbind(xx1, rep(Pi, 50), rep(0, length=length))
rr$points3d(rdat, col = 2, type = "l", lwd = 4)


# Pick 4 values along the x axis to plot the density curves.
xx2 <- c(0.2, 0.4, 0.6, 0.8)
sample_n <- 50
for (i in 1:length(xx2)){
  #The spikes at 0
  rdat1 <- c(xx2[i], 0, 0)
  rdat2 <- c(xx2[i], 0, 1-Pi)
  rdat  <- rbind(rdat1, rdat2)
  rr$points3d(rdat, col = 2, type = "l", lwd = 3)
  
  #The gamma curves
  Xi   <- c(1, xx2[i])
  mu.i <- exp(Xi %*% coef(G1))
  mu.i <- as.numeric(mu.i)
  Scale <- mu.i / Shape 
  Zi    <- Pi * dgamma(x = seq(0,30, length = sample_n), shape=Shape, scale = Scale)
  rdat <- cbind(rep(xx2[i], 50),seq(0,30, length = sample_n), Zi)
  rr$points3d(rdat, col = 2, type = "l", lwd = 3)
}

