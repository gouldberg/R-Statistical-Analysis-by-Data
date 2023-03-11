# ------------------------------------------------------------------------------
# Data: Ospreys
#
# Model:  model eggshell thickness as a function of DDD levels using bivariate linear regression and find a significant relationship
# DDD: a breakdown product of DDT (dichlorodiphenyltrichloroethane), a pesticide used in the past to control insects in agriculture
#
# OBJECTIVE:  Frequentist approach for OLS model, dance of p-value
#  - The frequentist approach assumes that 1. each regression paramter has unique value
#  - 2, using estimating techniques like OLS and MLE gives us the tools to estimate there parameters with a measure of uncertainty around these estimates
#  - 3. However, interpretation of the confidence intervals is based on fictive data  (but In reality, we are not repeating experiment)
#
# OBJECTIVE:  Try Bayesian approach (credible interval) by JAGS
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr", "Hmisc", "lattice", "ggplot2", "MASS", "VGAM", "car", "lme4")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

rm(list=ls())


# ------------------------------------------------------------------------------
# Load Data and Basics
# ------------------------------------------------------------------------------
data <- read.csv(file = "./ZuurZero-InflatedModelswithR/Ospreys.csv", header=T, dec=".")
dim(data)
str(data)

Hmisc::describe(data)

is.na(data)
colSums(is.na(data))


# ------------------------------------------------------------------------------
# Bivariate regression (OLS) model
# ------------------------------------------------------------------------------
M1 <- lm(THICK ~ DDD , data = data)

# DDD effect is rather weak:  we have a p-value of 0.0368
summary(M1)

par(mfrow = c(1,2), mar = c(5,5,2,2), cex.lab = 1.5)
plot(x = data$DDD, y = data$THICK, xlab = "DDD", ylab = "Eggshell thickness")

MyData <- data.frame(DDD = seq(min(data$DDD), max(data$DDD), length = 150))
P1 <- predict(M1, newdata = MyData, se = TRUE)
plot(x = data$DDD, y = data$THICK, xlab = "DDD", ylab = "Eggshell thickness")

tval <- abs(qt(0.05/2, 25 - 2))
lines(MyData$DDD, P1$fit, lwd = 3)
lines(MyData$DDD, P1$fit + tval * P1$se.fit, lwd = 3, lty = 2)
lines(MyData$DDD, P1$fit - tval * P1$se.fit, lwd = 3, lty = 2)


# Residuals
par(mfrow=c(1,1))
plot(resid(M1));  abline(h=0, col="gray", lty=2);
plot(x = fitted(M1), y = resid(M1));  abline(h=0, col="gray", lty=2);


# Residuals in OLS model
par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
plot(x = data$DDD, y = data$THICK, xlab = "DDD", ylab = "Eggshell thickness", pch = 16)
lines(MyData$DDD, P1$fit, lwd = 3)

for (i in 1:nrow(data)) {
  segments(x0 = data$DDD[i], y0 = fitted(M1)[i],x1 = data$DDD[i], y1 = data$THICK[i], lty = 2)
}


# ------------------------------------------------------------------------------
# Bivariate regression (OLS)
# What is p-value ?
# ------------------------------------------------------------------------------
# generate data based on the model
beta  <- coef(M1)
sigma <- summary(M1)$sigma

length <- 1000
MyData <- data.frame(DDD = seq(min(data$DDD), max(data$DDD), length = length))
Xp <- model.matrix(~ 1 + DDD, data = MyData)
MyData$THICK  <- rnorm(length, mean = Xp %*% beta, sd = sigma)
M2 <- lm(THICK ~ DDD, data = MyData)


# Adjusted R-squared is almost same but the standard errors (and tvalue and pvalue) is different
# The p-value quantifies how extreme the test statistic is, given the null hypothesis.  The p-value does not give the probability that the null hypothesis is true.
# The p-value is based on fictive (never observed) data and depends on the sample size !!!!
summary(M1)
summary(M2)


par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
plot(x = MyData$DDD, y = MyData$THICK, xlab = "DDD", ylab = "Eggshell thickness", pch = 1, main = "Simulated data", cex.main = 1.5)

MyData1 <- data.frame(DDD = seq(min(MyData$DDD), max(MyData$DDD), length = 150))
P2 <- predict(M2, newdata = MyData1, se = TRUE)
tval <- abs(qt(0.05/2, 1000 - 2))
lines(MyData1$DDD, P2$fit, lwd = 5)
lines(MyData1$DDD, P2$fit + tval * P1$se.fit, lwd = 5, lty = 2)
lines(MyData1$DDD, P2$fit - tval * P1$se.fit, lwd = 5, lty = 2)


# ------------------------------------------------------------------------------
# Bivariate regression (OLS)
# 95% confidence interval of slope beta:  Out of the 100 datasets we would have expected the value of 0 within the 95% confidence interval to happen about 5% of the time.
#  - This is based on the condition that the underlying assumptions like homogeneity, independence, and normality hold.
# ------------------------------------------------------------------------------
# Simulation study showing the 100 95% confidence intervals
set.seed(345678)
tval   <- abs(qt(0.05/2, 25 - 2))
MyData <- data.frame(DDD = data$DDD)

sim_n <- 100
OUT  <- matrix(nrow = sim_n, ncol = 4)
PVAL <- matrix(nrow = sim_n)
for (i in 1:sim_n) {
  Xp <- model.matrix(~ 1 + DDD, data = data)
  MyData$THICK  <- rnorm(nrow(data), mean = Xp %*% beta, sd = sigma)
  Mi <- lm(THICK ~ DDD, data = MyData)
  OUT[i,] <- c(coef(Mi)[2], sqrt(diag(vcov(Mi)))[2])
  PVAL[i] <- summary(Mi)[4]$coefficients[2,4]
}
OUT[,3] <- OUT[,1] - tval * OUT[,2]
OUT[,4] <- OUT[,1] + tval * OUT[,2]
OUT

par(mar = c(5,5,2,2))
plot(0,0, xlab = "Estimated parameter with 95% confidence interval", ylab = "100 realisations", cex.lab = 1.5, type = "n", axes = FALSE, xlim = c(-30, 10), ylim = c(0, sim_n+1))

for (i in 1:sim_n){
  segments(OUT[i,3], i, OUT[i,4], i)
  points(OUT[i,1], i, pch = 16, cex = 1.5)
}     
axis(2); axis(1); abline(v = 0, lty = 2);

SE <- sqrt(diag(vcov(M1)))[2]
segments(beta[2] - tval * SE, 0, beta[2] + tval * SE, 0, col = 1)
points(beta[2], 0, pch = 16, col = 1, cex = 1.5)
box()


# Dance of the p-values
par(mar = c(5,5,2,2), cex.lab = 1.5)
plot(x = PVAL, y = 1:100, xlab = "p-value", ylab = "100 realisations", pch = 16)
abline(v = 0.05, lty = 2)


# ------------------------------------------------------------------------------
# Bivariate regression (OLS)
# 
# Simulated value from a normal distribution, where the mean and variance is given by the OLS model
# ------------------------------------------------------------------------------
# Picture for Normal distribution
par(mar = c(5,5,2,2), cex.lab = 1.5)
plot(x = data$DDD, y = data$THICK, xlab = "DDD", ylab = "Eggshell thickness", pch = 16, cex = 2, ylim = c(30, 65))

length <- 10
MyData <- data.frame(DDD = seq(min(data$DDD), max(data$DDD), length = length))
P1 <- predict(M1, newdata = MyData, se = FALSE)
lines(MyData$DDD, P1, lwd = 3)

for (i in 1:length){
  points(rep(MyData$DDD[i], nrow(data)),
         rnorm(nrow(data), mean = P1[i], sd = sigma),
         pch = 1)
}


# 3D picture for a normal distribution
DDD <- seq(min(data$DDD), max(data$DDD), length = 30)
mu  <- beta[1] + beta[2] * DDD
Z   <- matrix(nrow = 30, ncol = 30)
qx  <- seq(35, 60, length = 30)
for (i in 1:30) Z[i,] <- dnorm(qx, mean = mu[i], sd = sigma)

persp(x = DDD, 
      y = qx, 
      z = Z,
      scale = TRUE,
      ylim = c(35, 60),
      xlim = c(0,1),
      theta = -125, 
      phi = 20, 
      expand = 1,
      ltheta = -520, shade = 0.8, 
      ticktype = "detailed",
      xlab = "DDD", 
      ylab = "Eggshell thickness", 
      zlab = "Probability",
      main = "")  -> res
round(res,3)
lines(trans3d(DDD, y = mu,z = rep(0,30), pmat = res), col = 1, lwd=5)
points(trans3d(data$DDD, data$THICK, rep(0,25), pmat = res), col = 1, pch = 16)


# ------------------------------------------------------------------------------
# Do it yourself MCMC
# FUNCTIONS
# ------------------------------------------------------------------------------
# Write a function to calculate the log likelihood for given alpha and beta
MyLogLik <- function(theta, THICK, DDD){
  alpha <- theta[1];  beta  <- theta[2];  sigma <- 5.168
  N <- length(THICK)
  L <- -sum(  1 / (2*sigma*sigma)   *  (THICK-alpha-beta * DDD)^2)  -  (N/2) * log(2*pi*sigma^2)
  L
}

# Function for the prior
MyPrior <- function(theta){
  alpha <- theta[1];  beta  <- theta[2];
  fprior.a <- (1/sqrt(2 * pi * 100)) * exp(-alpha^2/ (2*100))
  fprior.b <- (1/sqrt(2 * pi * 100)) * exp(-beta^2/ (2*100))
  fprior <- fprior.a * fprior.b
  log(fprior)
}

# Function for 'joining the club'
JointheClub <- function(logAlpha) {
  u    <- runif(1)
  logu <- log(u)	
  logu < logAlpha
}


# ------------------------------------------------------------------------------
# Do it yourself MCMC
# Standardize covariates
# ------------------------------------------------------------------------------
# MCMC algorithms tend to work much better if the continuous covariates are standardized
# Keeping covariates small and all withing the same range makes it easier for the computer to perform numerical optimization tasks.
# If you don't standardize, then most likely you will need a longer burn-in to get good mixing
data$DDD.std <- scale(data$DDD)
M3 <- lm(THICK ~ DDD.std, data = data)
summary(M3)


# ------------------------------------------------------------------------------
# Do it yourself MCMC
# ------------------------------------------------------------------------------
# Log-Likelihood given by 
beta <- coef(M3)
MyLogLik(theta = c(beta[1], beta[2]), data$THICK, data$DDD.std) 
logLik(M3)


# MCMC algorithm
# logAlpha = log( P(New candidate | data) / P(Current parameters | data) )
set.seed(12345)
NumberIter    <- 100000  #Get 5000 iterations
Theta.t       <- matrix(nrow = NumberIter, ncol = 2)
Theta.star    <- vector(length = 2)
current.Theta <- c(0, 0)  #Starting values
Theta.t[1,]   <- current.Theta

j <- 1
while (j < NumberIter){
  Theta.star[1] <- rnorm(1, Theta.t[j,1], 0.5)
  Theta.star[2] <- rnorm(1, Theta.t[j,2], 0.5)
  #Calculate log(R)
  logAlpha <- MyLogLik(Theta.star, data$THICK, data$DDD.std)  +  MyPrior(Theta.star) - MyLogLik(Theta.t[j,], data$THICK, data$DDD.std) - MyPrior(Theta.t[j,])

  #Should the new sample join?
  Join <- JointheClub(logAlpha)
  if (Join == TRUE) { 
    j <- j + 1
    Theta.t[j,] <- Theta.star 	
  }
} 


# Theta.t contains the chains for each parameter
head(Theta.t, 10)


# ------------------------------------------------------------------------------
# Do it yourself MCMC
# Plot the intercept and slope at each iteration (sample)
# ------------------------------------------------------------------------------
# plot intercept and slope for each iteration (sample)
par(mfrow = c(2, 1), mar = c(5, 5, 2, 2))
plot(x = 1:NumberIter, y = Theta.t[,1],  xlab = "Iterations", ylab = "Intercept", type = "l", cex.lab = 1.5)
abline(h = coef(M3)[1], lwd =2)
plot(x = 1:NumberIter, y = Theta.t[,2],  xlab = "Iterations", ylab = "Slope", type = "l", cex.lab = 1.5)
abline(h = coef(M3)[2], lwd =2)


# Zoom in on the first 5000 iterations
par(mfrow = c(2, 1), mar = c(5, 5, 2, 2))
plot(x = 1:1000, y = Theta.t[1:1000,1], xlab = "Iterations", ylab = "Intercept", type = "l", cex.lab = 1.5)
abline(h = coef(M3)[1], lwd =2)
plot(x = 1:1000, y = Theta.t[1:1000,2], xlab = "Iterations", ylab = "Slope", type = "l", cex.lab = 1.5)
abline(h = coef(M3)[2], lwd =2)


# Chains without the burn in
BurnIn      <- 10001  #Drop the first 500
par(mfrow = c(2, 1), mar = c(5, 5, 2, 2))
plot(x = BurnIn:NumberIter,           #From 501 to 5000
     y = Theta.t[BurnIn:NumberIter],  #From 501 to 5000
     xlab = "Iterations", ylab = "Intercept", type = "l", cex.lab = 1.5)
abline(h = coef(M3)[1], lwd =2)
plot(x = BurnIn:NumberIter, y = Theta.t[BurnIn:NumberIter,2],  xlab = "Iterations", ylab = "Slope",  type = "l", cex.lab = 1.5)
abline(h = coef(M3)[2], lwd =2)


# ------------------------------------------------------------------------------
# Do it yourself MCMC
# scatterplot of the MCMC first XXX iterations for the intercept and slope with arrow (after the burn-in)
# ------------------------------------------------------------------------------
par(mfrow=c(1,1))
par(mar = c(5,5,2,2), cex.lab = 1.5)

plot(x = Theta.t[BurnIn:NumberIter,1], y = Theta.t[BurnIn:NumberIter,2], xlab = "Intercept", ylab = "Slope", type = "n")
abline(v = mean(Theta.t[BurnIn:NumberIter,1]), col = 2, lty = 2) #Posterior mean intercept
abline(h = mean(Theta.t[BurnIn:NumberIter,2]), col = 2, lty = 2) #Posterior mean slope

beta1 <- Theta.t[BurnIn:NumberIter,1]
beta2 <- Theta.t[BurnIn:NumberIter,2]
a0 <- c(beta1[1], beta2[1])
sample_n <- 2:500
for (i in sample_n) {
  a1 <- c(beta1[i], beta2[i]) 
  arrows(a0[1], a0[2], a1[1], a1[2], length = 0)
  a0 <- a1       
}


# ------------------------------------------------------------------------------
# Do it yourself MCMC
# scatterplot of the MCMC iterations for the intercept and slope
# ------------------------------------------------------------------------------
scatterBarNorm <- function(x, dcol="blue", lhist=20, num.dnorm=5*lhist, ...){
  ## check input
  stopifnot(ncol(x)==2)
  ## set up layout and graphical parameters
  layMat <- matrix(c(2,0,1,3), ncol=2, byrow=TRUE)
  layout(layMat, widths=c(5/7, 2/7), heights=c(2/7, 5/7))
  ospc <- 0.5 # outer space
  pext <- 4 # par extension down and to the left
  bspc <- 1 # space between scatter plot and bar plots
  par. <- par(mar=c(pext, pext, bspc, bspc), oma=rep(ospc, 4)) # plot parameters
  ## scatter plot
  plot(x, xlim=range(x[,1]), ylim=range(x[,2]), ...)
  ## 3) determine barplot and height parameter
  ## histogram (for barplot-ting the density)
  xhist <- hist(x[,1], plot=FALSE, breaks=seq(from=min(x[,1]), to=max(x[,1]),length.out=lhist))
  yhist <- hist(x[,2], plot=FALSE, breaks=seq(from=min(x[,2]), to=max(x[,2]), length.out=lhist)) # note: this uses probability=TRUE
  ## determine the plot range and all the things needed for the barplots and lines
  xx <- seq(min(x[,1]), max(x[,1]), length.out=num.dnorm) # evaluation points for the overlaid density
  xy <- dnorm(xx, mean=mean(x[,1]), sd=sd(x[,1])) # density points
  yx <- seq(min(x[,2]), max(x[,2]), length.out=num.dnorm)
  yy <- dnorm(yx, mean=mean(x[,2]), sd=sd(x[,2]))
  ## barplot and line for x (top)
  par(mar=c(0, pext, 0, 0))
  barplot(xhist$density, axes=FALSE, ylim=c(0, max(xhist$density, xy)), space=0) # barplot
  lines(seq(from=0, to=lhist-1, length.out=num.dnorm), xy, col=dcol) # line
  ## barplot and line for y (right)
  par(mar=c(pext, 0, 0, 0))
  barplot(yhist$density, axes=FALSE, xlim=c(0, max(yhist$density, yy)), space=0, horiz=TRUE) # barplot
  lines(yy, seq(from=0, to=lhist-1, length.out=num.dnorm), col=dcol) # line
  ## restore parameters
  par(par.)
}

#require(mvtnorm)
#X <- rmvnorm(1000, c(0,0), matrix(c(1, 0.8, 0.8, 1), 2, 2))
X <- Theta.t[BurnIn:NumberIter,1:2]

#MyFile <- paste(MyDir, "Figure2_16.jpg", sep = "")
#jpeg(MyFile, quality = MyQuality)#, width = 650)

par(mar = c(5,5,2,2))
scatterBarNorm(X,  xlab = "Intercept", ylab = "Slope", cex.lab = 1.3)
#dev.off()


# ------------------------------------------------------------------------------
# Do it yourself MCMC
# Statistics:  Posterior mean values and posterior distributions
# The values are similar to the frequentist results
# ------------------------------------------------------------------------------
alpha1 <- mean(Theta.t[BurnIn:NumberIter,1], na.rm = TRUE)
beta1  <- mean(Theta.t[BurnIn:NumberIter,2], na.rm = TRUE)
alpha1.sd <- sd(Theta.t[BurnIn:NumberIter,1], na.rm = TRUE)
beta1.sd  <- sd(Theta.t[BurnIn:NumberIter,2], na.rm = TRUE)

# Compare home-made MCMC with glm/lm results
summary(M1)
summary(M3)

Z <- matrix(nrow = 2, c(alpha1, alpha1.sd, beta1, beta1.sd), byrow = TRUE)
colnames(Z) <- c("Posterior mean", "Posterior sd")
rownames(Z) <- c("Intercept", "Slope")
Z


# ------------------------------------------------------------------------------
# MCMC by JAGS
# ------------------------------------------------------------------------------
JAGS.data <- list(THICK = data$THICK, DDD = data$DDD, N = nrow(data))
JAGS.data

# JAGS modelling code
# we will use diffuse normal priors for beta.
# NOTE that JAGS needs "precision" = 1 / variance instead variance of the prior
# we will use diffuse uniform priors for standard deviations
sink("OspreysJAGS.txt")
cat(" 
    model{
    #1A. Priors beta and sigma
    for (i in 1:2) { beta[i] ~ dnorm(0, 0.0001)}
    
    tau  <- 1 / (sigma * sigma)
    sigma ~ dunif(0, 20)
    
    #2. Likelihood
    for (i in 1:N) {
    THICK[i] ~ dnorm(mu[i], tau)  
    mu[i]   <- beta[1] + beta[2] * DDD[i]   
    }
    }           
    ",fill = TRUE)
sink()


# Initial values & parameters to save
inits  <- function () list(beta  = rnorm(2, 0, 0.1), sigma = runif(0, 20))
params <- c("beta", "sigma")


# Start JAGS
library(R2jags)
J1 <- jags(data       = JAGS.data,
           inits      = inits,
           parameters = params,
           model      = "OspreysJAGS.txt",
           n.thin     = 10,
           n.chains   = 3,
           n.burnin   = 4000,
           n.iter     = 5000)

# update function considers all iterations before the update as burn-in, but it will continue where previous chains ended.
# In the case of poor mixing we can run the update function a couple of times.
# In this case we have 3(n.chains) * 50000(n.iter) / 10(n.thin) = 15,000 iterations for each posterior distribution.
J2  <- update(J1, n.iter = 50000, n.thin = 10)  

out <- J2$BUGSoutput
print(out, digits = 3)  

head(out$sims.matrix)


# ------------------------------------------------------------------------------
# MCMC by JAGS
# FUNCTIONS for Asessment
# ------------------------------------------------------------------------------
MyBUGSChains <- function(xx, vars, PanelNames = NULL){
  #Small function to make an xyplot of the iterations per chain,
  #for each variable 
  x <- xx$sims.array
  idchain.All <- NULL
  x1.All <- NULL
  ChainLength.All <- NULL
  id.All <- NULL
  
  NumBerChains <- ncol(x[,,vars[1]])
  
  for (i in vars){
    x1          <- as.vector(x[,,i])
    id          <- rep(rep(i, length = nrow(x[,,i])),NumBerChains)
    idchain     <- rep(1:NumBerChains, each = nrow(x[,,i]))
    ChainLength <- rep(1: nrow(x[,,i]), NumBerChains)
    
    x1.All <- c(x1.All, x1)
    ChainLength.All <- c(ChainLength.All, ChainLength)
    id.All <- c(id.All, id)
    idchain.All <- c(idchain.All, idchain)
  }
  
  
  if (!is.null(PanelNames)) { 
    if (length(unique(id.All)) != length(PanelNames)) {stop("Wrong number of panel names")}
    AllNames <- unique(id.All)
    for (i in 1:length(AllNames)) {
      id.All[id.All == AllNames[i]] <- PanelNames[i] 
    }
    id.All <- factor(id.All, levels = PanelNames)
  }
  
  Z <- xyplot(x1.All ~ ChainLength.All | factor(id.All) ,
              type = "l",
              strip = strip.custom(bg = 'white',par.strip.text = list(cex = 1.2)),
              scales = list(x = list(relation = "same", draw = TRUE), y = list(relation = "free", draw = TRUE)),
              groups = idchain.All,  col = 1:NumBerChains,
              xlab = list(label = "MCMC iterations", cex = 1.5),
              ylab = list(label = "Sampled values", cex = 1.5))
  print(Z)
}


MyBUGSOutput <- function(Output  = Output, SelectedVar = SelectedVar, VarNames = NULL){
  xx   <- Output
  vars <- SelectedVar
  
  if (is.null(VarNames)) { VarNames <- SelectedVar }
  if (length(SelectedVar) != length(VarNames)) {stop("Wrong number of variable names")}
  
  x <- xx$sims.matrix
  OUT <- matrix(nrow = length(vars), ncol=4) 
  j<-1
  for(i in vars){
    xi <- x[,i]	
    OUT[j,3:4] <- quantile(xi, probs = c(0.025, 0.975))
    OUT[j,1]   <- mean(xi)
    OUT[j,2]   <- sd(xi)
    j          <- j + 1
  }
  colnames(OUT) <- c("mean", "se", "2.5%", "97.5%")
  rownames(OUT) <- VarNames
  OUT
}


MyBUGSHist <- function(Output  = Output, SelectedVar = SelectedVar, PanelNames = NULL){
  #Small function to make an histogram of the ACF per chain.
  #xx$BUGSoutput is the out object from JAGS
  #vars is a character string of variables in xx
  #PanelNames are matching names for the panels
  
  #for each variable 
  x <- Output$sims.matrix
  AllParams <- NULL
  
  if (is.null(PanelNames)) { PanelNames <- SelectedVar }
  if (length(SelectedVar) != length(PanelNames)) {stop("Wrong number of panel names")}
  
  
  for (i in SelectedVar){
    #Extract data from variable i
    Paramsi <- x[,i]
    AllParams <- c(AllParams, Paramsi)	
  }
  
  #AllID <- rep(vars, each = nrow(x))
  AllID2 <- rep(PanelNames, each = nrow(x))
  AllID2 <- factor(AllID2, levels = PanelNames)
  
  
  MyPanelCex <- 1.2
  Z <- histogram( ~ AllParams | factor(AllID2),
                  strip = strip.custom(bg = 'white', par.strip.text = list(cex = MyPanelCex)),
                  type = "count" ,
                  nint = 100,
                  xlab = list(label = "Posterior distribution", cex = 1.5),
                  col = gray(0.5), 
                  ylab = list(label = "Frequencies", cex = 1.5),
                  scales = list(alternating = FALSE, 
                                x = list(relation = "free"),
                                y = list(relation = "free")),
                  breaks=NULL,              
                  panel = function(x, ...) {
                    panel.histogram(x, ...)
                    panel.abline(v = 0, lwd = 3, col =2)
                    CI <- quantile(x, probs = c(0.025,0.975))
                    panel.arrows (CI[1],-2, CI[2],-2, col = 2, lwd= 7, length=0)
                  })
  print(Z)
}


# ------------------------------------------------------------------------------
# MCMC by JAGS
# Asess mixing
# ------------------------------------------------------------------------------
MyNames <- c("Intercept", "Slope", "sigma")
MyBUGSChains(out, c("beta[1]", "beta[2]", "sigma"), PanelNames = MyNames)


# ------------------------------------------------------------------------------
# MCMC by JAGS
# Posterior Information
# ------------------------------------------------------------------------------
MyNames <- c("Intercept", "Slope", "sigma")
OUT1 <- MyBUGSOutput(out,  c("beta[1]", "beta[2]", "sigma"), VarNames = MyNames)
print(OUT1, digits = 5)


# Postrior distribution
MyBUGSHist(out, c("beta[1]", "beta[2]", "sigma"), PanelNames = MyNames)


# Posterior distribution of beta and sigma
beta1 <- out$sims.matrix[,'beta[1]']
beta2 <- out$sims.matrix[,'beta[2]']
sigma <- out$sims.matrix[,'sigma']
par(mfrow = c(3,1), mar = c(5,5,2,2))
hist(beta1, xlab = "Posterior distribution of the intercept", ylab = "Probability", main = "", probability = TRUE, cex.lab = 1.5, breaks = 100)
hist(beta2, xlab = "Posterior distribution of the slope", ylab = "Probability", main = "", probability = TRUE, cex.lab = 1.5, breaks = 100)
hist(sigma, xlab = "Posterior distribution of sigma", ylab = "Probability", main = "", probability = TRUE, cex.lab = 1.5, breaks = 100)

quantile(beta2, probs = c(0.25, 0.75))
quantile(beta2, probs = c(0.025, 0.975))


m1 <- c(mean(beta1), mean(beta2), mean(sigma))
sd1 <- c(sd(beta1), sd(beta2), sd(sigma))
Z <-cbind(m1, sd1)
colnames(Z) <- c("Posterior mean", "Posterior sd")
rownames(Z) <- c("beta 1", "beta 2", "sigma")
Z
