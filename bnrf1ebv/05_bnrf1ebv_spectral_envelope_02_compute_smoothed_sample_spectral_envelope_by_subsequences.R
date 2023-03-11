setwd("//media//kswada//MyFiles//R//bnrf1ebv")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  bnrf1ebv
# ------------------------------------------------------------------------------

data(bnrf1ebv, package = "astsa")

bnrf1ebv



# ------------------------------------------------------------------------------
# Divide into subsequences
# ------------------------------------------------------------------------------

# divide into subsequences

x1 <- x[1:1000,]
x2 <- x[1001:2000,]
x3 <- x[2001:3000,]
x4 <- x[3001:3954,]


xsub <- list(x1 = x1, x2 = x2, x3 = x3, x4 = x4) 



# ------------------------------------------------------------------------------
# Compute spec envelope and scale vectors for each subsequence
# ------------------------------------------------------------------------------

comp_specenv <- function(x){

  xspec <- mvspec(x, spans = c(7, 7), plot = FALSE)
  
  ( fxxr <- Re(xspec$fxx) )
  
  # ----------
  # compute Q = Var ^ (-0.5)
  
  ev <- eigen(var(x))
  
  Q <- ev$vectors %*% diag(1 / sqrt(ev$values)) %*% t(ev$vectors)
  
  # ----------
  # compute spec envelope and scale vectors
  
  # sample size used for FFT
  num <- xspec$n.used
  
  # number of freqs used
  nfreq <- length(xspec$freq)
  
  # initialize the spec envelope
  specenv <- matrix(0, nfreq, 1)
  
  # initialize the scale vectors
  beta <- matrix(0, nfreq, 3)
  
  for(k in 1:nfreq){
    ev <- eigen(2 * Q %*% fxxr[,,k] %*% Q / num, symmetric = TRUE)
    
    # spec env at freq k/n is max evalue
    specenv[k] <- ev$values[1]
    
    # beta at freq k/n
    b <- Q %*% ev$vectors[,1]
    
    # helps to normalize beta
    beta[k,] <- b / sqrt(sum(b ^ 2))
  }
  
  return(list(xspec = xspec, specenv = specenv))
}



output <- lapply(1:length(xsub), function(x) comp_specenv(xsub[[x]]))


output



  
# ------------------------------------------------------------------------------
# Output and graphics
# ------------------------------------------------------------------------------

graphics.off()

par(mfrow = c(2,2))

for(i in 1:length(xsub)){
  
  frequency <- output[[i]]$xspec$freq
  
  specenv <- output[[i]]$specenv
  
  # Smoothed sample spectral envelope of the BNRF1 gene from the Epstein-Varr virus
  plot(frequency, 100 * specenv, type = "l", ylab = "Spectral Envelope (%)", ylim = c(0, 1.4))
  
  
  # ----------
  # add significance threshold to plot
  m <- output[[i]]$xspec$kernel$m
  
  etainv <- sqrt(sum(output[[i]]$xspec$kernel[-m:m] ^ 2))
  
  num <- output[[i]]$xspec$n.used
  
  thresh <- 100 * (2 / num) * exp(qnorm(.9999) * etainv)
  
  abline(h = thresh, lty = 6, col = 4)
}



# -->
# An approximate 0.0001 significance threshold is 0.69%.

# The first 3 quarters contain the signal at the frequency 1/3
