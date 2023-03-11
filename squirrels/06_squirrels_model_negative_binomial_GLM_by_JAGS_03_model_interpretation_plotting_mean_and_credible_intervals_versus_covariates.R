setwd("//media//kswada//MyFiles//R//squirrels")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Red Squirrels
# ------------------------------------------------------------------------------

SQ <- read.table(file = "RedSquirrels.txt", header = TRUE, dec = ".")


str(SQ)


# ----------
# remove large DBH
SQ2 <- subset(SQ, DBH < 0.6)



# ------------------------------------------------------------------------------
# Scaling covariate  (to compare with MCMC result)
# ------------------------------------------------------------------------------

SQ2$Ntrees.std      <- as.numeric(scale(SQ2$Ntrees))

SQ2$TreeHeight.std  <- as.numeric(scale(SQ2$TreeHeight))

SQ2$CanopyCover.std <- as.numeric(scale(SQ2$CanopyCover))



# ------------------------------------------------------------------------------
# Fitting negative binomial GLM model in JAGS
# For enhance model interpretation, we add codes
# ------------------------------------------------------------------------------

sink("GLMNB2.txt")

cat("
    model{
    #1. Priors for regression coefficients
    for (i in 1:K) { beta[i] ~ dnorm(0, 0.0001) } 
    #Prior for size
    size ~ dunif(0.001, 5)
    
    #2. Likelihood function
    for (i in 1:N){  
    SqCones[i]  ~  dnegbin(p[i], size)
    p[i]       <- size / (size + mu[i])  
    log(mu[i]) <- eta[i]
    eta[i]     <- inprod(beta[], X[i,])
    
    #Discrepancy measures (used for checking overdispersion)
    YNew[i]   ~ dnegbin(p[i], size)   #New data
    expY[i]    <- mu[i] 
    varY[i]    <- mu[i] + pow(mu[i],2) / size
    PRes[i]    <- (SqCones[i]  - expY[i]) / sqrt(varY[i])
    PResNew[i] <- (YNew[i] - expY[i]) / sqrt(varY[i])
    D[i]       <- pow(PRes[i], 2)
    DNew[i]    <- pow(PResNew[i], 2)
    
    MUf1[i] <- exp(beta[1] + beta[2] * X[i,2])
    MUf2[i] <- exp(beta[1] + beta[3] * X[i,3])
    MUf3[i] <- exp(beta[1] + beta[4] * X[i,4])
    pf1[i] <- size / (size + MUf1[i])  
    Yf1[i]   ~ dnegbin(pf1[i], size) 
    }    
    Fit         <- sum(D[1:N])
    FitNew      <- sum(DNew[1:N])
    }
    ",fill = TRUE)

sink()



# ----------
# Set the initial values for the betas and sigma
inits <- function () {
  list(
    beta = rnorm(K, 0, 0.1),
    size = runif(0.001, 5)
  )  }



# ----------
# Parameters to estimate
params <- c("beta", "Fit", "FitNew", "size", "PRes",
            "MUf1", "MUf2", "MUf3")



# ----------
K1 <- jags(data = win.data, 
           inits = inits, 
           parameters.to.save = params,
           model.file = "GLMNB2.txt",
           n.chains = 3,
           n.iter = 5000,
           n.thin = 10,
           n.burnin = 4000)


K2 <- update(K1, n.iter = 10000, n.thin = 10)           




# -----------
out  <- K2$BUGSoutput

OUT1 <- MyBUGSOutput(out, uNames("MUf1",51))

print(OUT1, digits = 3)



# ----------
# plotting fitted and 95% credible intervals versus each covariate

par(mfrow = c(1,3), mar = c(5,5,2,2))

OUT1 <- MyBUGSOutput(out, uNames("MUf1",51))

CV <- sort(win.data$X[,"Ntrees.std"])

I1 <- order(win.data$X[,"Ntrees.std"])

plot(x = CV, OUT1[I1,1], type = "l",
     ylim = c(0,30),
     xlab = "Standardized number of trees",
     ylab = "Predicted values",
     cex.lab = 2)

lines(x=CV, OUT1[I1,3], type = "l", lty = 2)

lines(x=CV, OUT1[I1,4], type = "l", lty = 2)



# ----------
OUT1 <- MyBUGSOutput(out, uNames("MUf2",51))

CV <- sort(win.data$X[,"TreeHeight.std"])

I1 <- order(win.data$X[,"TreeHeight.std"])

plot(x=CV, OUT1[I1,1], type = "l",
     ylim = c(0,30),
     xlab = "Standardized tree height",
     ylab = "Predicted values",
     cex.lab = 2 )

lines(x=CV, OUT1[I1,3], type = "l", lty = 2)

lines(x=CV, OUT1[I1,4], type = "l", lty = 2)



# ----------
OUT1 <- MyBUGSOutput(out, uNames("MUf3",51))

CV <- sort(win.data$X[,"CanopyCover.std"])

I1 <- order(win.data$X[,"CanopyCover.std"])

plot(x=CV, OUT1[I1,1], type = "l",
     ylim = c(0,30),
     xlab = "Standardized canopy cover",
     ylab = "Predicted values",
     cex.lab =2 )

lines(x=CV, OUT1[I1,3], type = "l", lty = 2)

lines(x=CV, OUT1[I1,4], type = "l", lty = 2)


