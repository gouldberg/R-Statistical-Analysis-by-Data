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
# O'Sullivan splines in JAGS:  model
#   - Wand and Ormerod (2008) explain O'Sullivan splines and mention that O'Sullivan splines seem to perform best of all smoothers
#     (in terms of mixing in MCMC)
#   - O'Sullivan spline is closely related to a B-spline, and that just as with the low rank thin-plate regression spline,
#     the smoother can be written as a linear mixed effects model X * beta + Z * u, where the u are random intercepts that are assumed to be normally
#     distributed with mean zero and vairance sigma-u^2
# ------------------------------------------------------------------------------


OSullivan <- function(x, numIntKnots = 20, AddIntercept = FALSE, intKnots) {

  library(splines)  	
  #x is the covariate for the smoother
  #y is response variable
  a <- min(x) 
  b <- max(x)
  xg <- seq(a, b, length = 101)
  
  names(intKnots) <- NULL
  B <- bs(x, 
          knots = intKnots, 
          degree = 3,
          Boundary.knots = c(a, b),
          intercept = TRUE)
  
  
  #Create the Omega matrix:
  formOmega <- function(a,b,intKnots) {
    allKnots <- c(rep(a,4),intKnots,rep(b,4)) 
    K <- length(intKnots) ; L <- 3*(K+8)
    xtilde <- (rep(allKnots,each=3)[-c(1,(L-1),L)]+ 
                 rep(allKnots,each=3)[-c(1,2,L)])/2
    wts <- rep(diff(allKnots),each=3)*rep(c(1,4,1)/6,K+7)
    Bdd <- spline.des(allKnots,xtilde,derivs=rep(2,length(xtilde)),
                      outer.ok=TRUE)$design  
    Omega     <- t(Bdd*wts)%*%Bdd     
    return(Omega)
  }
  
  Omega <- formOmega(a,b,intKnots)
  eigOmega <- eigen(Omega)
  
  #Obtain the matrix for linear transformation of $\\bB$ to $\\bZ$:
  indsZ <- 1:(numIntKnots+2)
  UZ    <- eigOmega$vectors[,indsZ]
  LZ    <- t(t(UZ)/sqrt(eigOmega$values[indsZ]))
  
  #Perform stability check:  
  indsX <- (numIntKnots+3):(numIntKnots+4)
  UX    <- eigOmega$vectors[,indsX]   
  L     <- cbind( UX, LZ )
  stabCheck <- t(crossprod(L,t(crossprod(L,Omega))))          
  if (sum(stabCheck^2) > 1.0001*(numIntKnots+2))
    print("WARNING: NUMERICAL INSTABILITY ARISING FROM SPECTRAL DECOMPOSITION")
  
  # Form the X and Z matrices:
  X <- cbind(rep(1,length(x)),x)
  Z <- B %*%LZ
  
  if (!AddIntercept) { 
    X <- X[,-1]
    X <- as.matrix(X, ncol = 1) }
  
  list(X = X, Z = Z)
}



Xcov <- model.matrix(~Lat.std, data = Squid)    

numIntKnots <- 11

probs <- seq(0,1,length=(numIntKnots+2))[-c(1,(numIntKnots+2))]

intKnotsTime <- quantile(unique(Squid$ML.std), probs)

XZML <- OSullivan(Squid$ML.std, 
                  numIntKnots = numIntKnots, 
                  AddIntercept = FALSE,
                  intKnots = intKnotsTime)


# using univariate Normal priors may be better then multivariate Normal priors ...

win.data3 <- list(Y      = Squid$d15N,    #Response variable
                  Xcov   = Xcov,          #Covariates  
                  N      = nrow(Squid),   #Sample size
                  M      = ncol(Xcov),    #Betas 
                  Xspl   = XZML$X[,1],    #Basis for smoother 
                  Zspl   = XZML$Z,
                  Mz     = ncol(XZML$Z)   #u for smoother basis
)

win.data3




# ----------
# model
sink("GAMOSS.txt")

cat("
model{
    #1A. Priors regression parameters
    for (i in 1:M)  { beta[i] ~ dnorm(0, 0.0001) }  
    for (i in 1:Mz) { u[i] ~ dnorm(0, tau.u) }  
    b ~ dnorm(0, 0.0001)
   
    #1B. Prior for variance for epsilon
    num   ~ dnorm(0, 0.0016) 
    denom ~ dnorm(0, 1)
    sigma <- abs(num / denom) 
    tau   <- 1 / (sigma * sigma)
       
    #Priors for variance random intercept u for smoother
    num.u   ~ dnorm(0, 0.0016) 
    denom.u ~ dnorm(0, 1)
    sigma.u <- abs(num.u / denom.u) 
    tau.u   <- 1 / (sigma.u * sigma.u)
        
    #2. Likelihood
    for (i in 1:N) {
       Y[i]   ~ dnorm(mu[i], tau)
       mu[i] <- inprod(beta[], Xcov[i,]) + F1[i]         
       F1[i] <- Xspl[i] * b + inprod(Zspl[i,], u[]) 
              
       #3. Discrepancy measures   
       E[i] <- Y[i] - mu[i]
    }     
 }
",fill = TRUE)

sink()



# ----------
# Inits function
inits3  <- function () {
  list(beta  = rnorm(ncol(Xcov), 0, 0.1),  #Regression parameters
       b     = rnorm(1, 0, 0.01),
       u     = rnorm(ncol(XZML$Z), 0, 0.1),#Regression terms smoother
       num   = rnorm(1, 0, 25),            #Prior stuff for variance epsilon
       denom = rnorm(1, 0, 1),             #Prior stuff for variance epsilon
       num.u   = rnorm(1, 0, 25),          #Prior stuff for variance u
       denom.u = rnorm(1, 0, 1)            #Prior stuff for variance u
  )  }



# ----------
params3 <- c("beta", "E", "sigma", "b", "u", "sigma.u")



# ------------------------------------------------------------------------------
# O'Sullivan splines in JAGS:  Start gibbs sampler
# ------------------------------------------------------------------------------

K1   <- jags(data       = win.data3,
             inits      = inits3,
             parameters = params3,
             model      = "GAMOSS.txt",
             n.thin     = 10,
             n.chains   = 3,
             n.burnin   = 15000,
             n.iter     = 115000)


print(K1)



# ------------------------------------------------------------------------------
# O'Sullivan splines in JAGS:  assess model
#   - chain mixing
#   - ACF per chain
#   - parameter distribution
# ------------------------------------------------------------------------------

out <- K1$BUGSoutput

MyBUGSChains(out, c(uNames("beta", ncol(Xcov)), uNames("u", 3), "b", "sigma", "sigma.u"))

MyBUGSACF(out, uNames("beta", ncol(Xcov)))

MyBUGSHist(out, uNames("beta", ncol(Xcov)))


# ----------
OUT1 <- MyBUGSOutput(out, c(uNames("beta", ncol(Xcov)),"sigma", "sigma.u"))
print(OUT1, digits =3)



# Use for residual plots:
E <- out$mean$E



# ------------------------------------------------------------------------------
# O'Sullivan splines in JAGS:  sketch smoothers
# ------------------------------------------------------------------------------

b <- out$sims.list$b  

u <- out$sims.list$u


# This depends on the selected data!
range(Squid$ML.std)
ML.100 <- seq( -1.4,  3, length = 100)



# ----------
# Convert this covariate into a smoother basis
XZML100 <- OSullivan(ML.100, 
                     numIntKnots = numIntKnots, 
                     AddIntercept = FALSE,
                     intKnots = intKnotsTime)

Z100 <- XZML100$Z


# Calculate the smoothers
f1 <-  ML.100 %*% t(b) + Z100 %*% t(u) 



# Get posterior mean and 95% credible intervals
f1.info <- MySmoother(f1)



# ----------
par(mar = c(5,5,2,2), cex.lab = 1.5)  
plot(x = ML.100, 
     y = f1.info[,4], 
     type = "l", 
     xlab = "Standardized ML",
     ylab = "Smoother",
     ylim = c(-2,2), cex.lab = 1.5)
lines(ML.100, f1.info[,1], lty=2)
lines(ML.100, f1.info[,3], lty=2)
