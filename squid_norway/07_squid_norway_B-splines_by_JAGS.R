setwd("//media//kswada//MyFiles//R//squid_norway")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Squid Norway
# ------------------------------------------------------------------------------
Squid <- read.table(file = "SquidNorway.txt", header = TRUE)


str(Squid)



# ----------
# for comparison with MCMC approach later, we standardize the covariate
Mystd <- function(x) {(x-mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)}

Squid$Lat.std <- Mystd(Squid$Lat)
Squid$ML.std  <- Mystd(Squid$ML)



# ------------------------------------------------------------------------------
# B-splines in JAGS:  model
#   - To deal with the collinearity of the terms in the basis special matrix multiplicaionts can be applied to reduce the correlation.
#     The resulting smoother is called a B-spline
#   - The problem with B-splie smoothers is that for more complicated distributions and dependency structures mixing in MCMC is not good
# ------------------------------------------------------------------------------

I1      <- order(Squid$ML.std)

Squid2 <- Squid[I1,]

X      <- model.matrix(~Lat.std, data = Squid2)    

library(splines)
X.bs  <- bs(Squid2$ML.std, knots = probs[2:4], intercept = FALSE)
K     <- ncol(X.bs)

win.data2 <- list(Y            = Squid2$d15N,      #Response variable
                  X            = X,                #Covariates  
                  N            = nrow(Squid2),     #Sample size
                  X.bs         = X.bs,             #Basis for smoother 
                  M            = ncol(X),
                  K            = ncol(X.bs)
)


win.data2



# ----------
# model
sink("SquidGAM2.txt")

cat("
model{
    #1A. Priors regression parameters
    for (i in 1:M) {beta[i] ~ dnorm(0, 0.0001)}  
    for (i in 1:K) {b[i]    ~ dnorm(0, 0.0001)}  
   
    #1B. Prior for variance for epsilon
    num   ~ dnorm(0, 0.0016) 
    denom ~ dnorm(0, 1)
    sigma <- abs(num / denom) 
    tau   <- 1 / (sigma * sigma)
       
    #2. Likelihood
    for (i in 1:N) {
       Y[i]  ~ dnorm(mu[i], tau)
       # for the intercept and slope of latitude
       mu[i] <- inprod(X[i,], beta[]) + F1[i]
       # for the smoother
       F1[i] <- inprod(X.bs[i,], b[])  
              
       #3. Discrepancy measures   
       E[i] <- Y[i] - mu[i]
    }     
 }
",fill = TRUE)

sink()



# ----------
# Inits function

inits2  <- function () {
  list(beta  = rnorm(ncol(X), 0, 0.01),        #Regression parameters
       b     = rnorm(ncol(X.bs), 0, 0.01), #Regression terms smoother
       num   = rnorm(1, 0, 25),          #Prior stuff for variance epsilon
       denom = rnorm(1, 0, 1)            #Prior stuff for variance epsilon
  )  }





# ----------
# Parameters to estimate
params2 <- c("beta", "E", "sigma", "b", "F1")



# ------------------------------------------------------------------------------
# B-splines in JAGS:  Start gibbs sampler
# ------------------------------------------------------------------------------
K1   <- jags(data       = win.data2,
             inits      = inits2,
             parameters = params2,
             model      = "SquidGAM2.txt",
             n.thin     = 10,
             n.chains   = 3,
             n.burnin   = 14000,
             n.iter     = 15000)

K2 <- update(K1, n.iter = 20000) 


print(K2, digits = 2)



# ------------------------------------------------------------------------------
# B-splines in JAGS:  assess model
#   - chain mixing
#   - ACF per chain
#   - parameter distribution
# ------------------------------------------------------------------------------

out <- K2$BUGSoutput


# ----------
MyBUGSChains(out, c(uNames("beta", ncol(X)), uNames("b", ncol(X.bs))))


# ----------
MyBUGSACF(out, uNames("beta", ncol(X)))



# ----------
OUT2 <- MyBUGSOutput(out, c(uNames("beta", ncol(X)), "sigma"))

print(OUT2, digits =3)



# Access posterior mean residuals
( E <- out$mean$E )

MyBUGSHist(out, uNames("beta", ncol(X)))



# ------------------------------------------------------------------------------
# B-splines in JAGS:  Sketch smoothers
# ------------------------------------------------------------------------------

# Extract coefficients
b <- out$sims.list$b

dim(b)

NewData <- data.frame(Lat.std = 0,
                      ML.std =  seq( -1.4,  3, length = 100))

X.bs  <- bs(NewData$ML.std, knots = probs[2:4], intercept = FALSE)



# ----------
# Calculate the smoothers
( f <-  X.bs %*% t(b) )
#--->>> Why is the first row equal to 0????


f.info <- MySmoother(f)



# ----------
# Plot the smoothers
par(mar = c(5,5,2,2))  
plot(x = NewData$ML.std, 
     y = f.info[,4], 
     type = "l", 
     xlab = "Standardized ML",
     ylab = "Smoother",
     ylim = c(-1,4),
     cex.lab= 1.5, lwd = 2)
lines(NewData$ML.std, f.info[,1], lty=2, lwd = 2)
lines(NewData$ML.std, f.info[,3], lty=2, lwd = 2)



# -->
# Odd!!! Better not do this?
