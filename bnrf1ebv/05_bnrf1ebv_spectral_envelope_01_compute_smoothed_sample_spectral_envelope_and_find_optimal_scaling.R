setwd("//media//kswada//MyFiles//R//bnrf1ebv")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  bnrf1ebv
# ------------------------------------------------------------------------------

data(bnrf1ebv, package = "astsa")

bnrf1ebv



# ------------------------------------------------------------------------------
# Convert to indicator matrix
# ------------------------------------------------------------------------------

# First, input the data as factors and then
u <- factor(bnrf1ebv)


# make an indicator matrix
x <- model.matrix(~ u - 1)[, 1:3]

head(x)


# select subsequence if desired
# x <- x[1:1000,]


# variance-covariance matrix
Var <- var(x)



# ------------------------------------------------------------------------------
# Compute spec envelope and scale vectors
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))
xspec <- mvspec(x, spans = c(7, 7))


( fxxr <- Re(xspec$fxx) )



# ----------
# compute Q = Var ^ (-0.5)

ev <- eigen(Var)


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



# ------------------------------------------------------------------------------
# Output and graphics
# ------------------------------------------------------------------------------

frequency <- xspec$freq

# Smoothed sample spectral envelope of the BNRF1 gene from the Epstein-Varr virus
plot(frequency, 100 * specenv, type = "l", ylab = "Spectral Envelope (%)")



# ----------
# add significance threshold to plot
m <- xspec$kernel$m

etainv <- sqrt(sum(xspec$kernel[-m:m] ^ 2))

thresh <- 100 * (2 / num) * exp(qnorm(.9999) * etainv)

abline(h = thresh, lty = 6, col = 4)



# ----------
# details

output <- cbind(frequency, specenv, beta)

colnames(output) <- c("freq", "specenv", "A", "C", "G")

head(round(output, 3))



# ----------
which.max(data.frame(output)$specenv)

data.frame(output)[which.max(data.frame(output)$specenv),]


# -->
# Strong signal at frequency 1/3
# The corresponding optimal scaling was A = 0.10, C = 0.61, G = 0.78, T = 0,
# which indicats the signal is in the strong-weak bonding alphabet, S = {C, G} and W = {A, T}




