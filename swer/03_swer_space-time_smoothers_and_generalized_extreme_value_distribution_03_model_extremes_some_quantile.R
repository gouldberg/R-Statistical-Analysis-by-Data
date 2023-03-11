setwd("//media//kswada//MyFiles//R//swer")

packages <- c("dplyr", "lattice", "gamair")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# This tutorial is based on
# Chapter 7. GAMs in Practive: mgcv from "Generalized Additive Models An Introduction with R 2nd Edition" (by Simon N. Wood)


# ------------------------------------------------------------------------------
# data:  Extreme rainfall in Switzerland
# ------------------------------------------------------------------------------

data(swer, package = "gamair")

str(swer)


head(swer)

car::some(swer)



# ------------------------------------------------------------------------------
# Model extremes some quantile of the fitted distribution
#   - Simulating from the approximate posterior distribution of the model coefficients, beta,
#     and then simulating replicate data from the resulting GEV.
# ------------------------------------------------------------------------------

# GEV inverse cdf.
Fi.gev <- function(z, mu, sigma, xi){

  # approximate xi = 0, by small xi  
  xi[abs(xi) < 1e-8] <- 1e-8
  
  x <- mu + ((-log(z)) ^ -xi - 1) * sigma / xi
}



# ----------
# posterior mean and cov
( mb <- coef(b) )

( Vb <- vcov(b) )



# ----------
# copy fitted model object to modify
b1 <- b



# ----------
n.rep <- 1000


# posterior sim
( br <- rmvn(n.rep, mb, Vb) )


n <- length(fitted(b))


sim.dat <- cbind(data.frame(rep(0, n * n.rep)), swer$code)



# ----------
# IT TAKES TIME :  2-3 min.
for(i in 1:n.rep){
  
  # copy sim coefs to gam object
  b1$coefficients <- br[i,]
  
  X <- predict(b1, type = "response")

  ii <- 1:n + (i - 1) * n
  
  sim.dat[ii,1] <- Fi.gev(runif(n), X[,1], exp(X[,2]), X[,3])
}



# ----------
# Mean and 98th percentile of the annual maximum for each station
( stm <- tapply(sim.dat[,1], sim.dat[,2], mean) )

( st98 <- tapply(sim.dat[,1], sim.dat[,2], quantile, probs = 0.98) )



# ----------
graphics.off()
par(mfrow=c(1,2))

plot(stm, tapply(swer$exra, swer$code, mean), xlab = "mean annual maximum", ylab = "simulated version")

plot(st98, tapply(swer$exra, swer$code, max), xlab = "maximum", ylab = "simulated 98th percentile")


