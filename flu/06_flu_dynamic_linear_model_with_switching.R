setwd("//media//kswada//MyFiles//R//flu")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  flu
# ------------------------------------------------------------------------------

data(flu, package = "astsa")

str(flu)

flu



# ------------------------------------------------------------------------------
# Dynamic Linear Model with Switching
#   - The results of this analysis are impressive given the small number of parameters and the degree of approximation that
#     was made to obtain a computationally simple method for fitting a complex model
# ------------------------------------------------------------------------------

y <- as.matrix(flu)

num <- length(y)

nstate <- 4


# obs matrix normal
M1 <- as.matrix(cbind(1, 0, 0, 1))


# obs matrix flu epi
M2 <- as.matrix(cbind(1, 0, 1, 1))


# to store pi2(t | t-1) and y(t | t-1)
prob <- matrix(0, num, 1)


yp <- y



# to store x(t | t)
xfilter <- array(0, dim = c(nstate, 1, num))



# ----------
# Function to calculate likelihood

Linn <- function(para){
  
  # The first component:  AR(2) process to represent the periodic (seasonal) component
  alpha1 <- para[1]
  alpha2 <- para[2]
  sQ1 <- para[4]
  
  # The second component:  AR(1) process with a nonzero constant term to represent the shapr rise during emidemic
  # here beta1 is removed (--> only beta0 for simple level shift)
  beta0 <- para[3]
  sQ2 <- para[5]
  
  
  like <- 0
  
  # filter
  xf <- matrix(0, nstate, 1)

  # x pred
  xp <- matrix(0, nstate, 1)
  
  # filter cov
  Pf <- diag(0.1, nstate)
  
  # pred cov
  Pp <- diag(0.1, nstate)
  
  # Marcov Chain Transition probabilities
  pi11 <- 0.75 -> pi22
  pi12 <- 0.25 -> pi21
  
  # Marcov Chain Transition initial values
  pif1 <- 0.5 -> pif2
  
  phi <- matrix(0, nstate, nstate)
  phi[1,1] <- alpha1
  phi[1,2] <- alpha2
  phi[2,1] <- 1
  phi[4,4] <- 1
  
  Ups <- as.matrix(rbind(0, 0, beta0, 0))
  
  Q <- matrix(0, nstate, nstate)
  Q[1,1] <- sQ1^2
  Q[3,3] <- sQ2^2
  
  # R = 0 in final model
  R <- 0
  
  # begin filtering
  for(i in 1:num){
    xp <- phi %*% xf + Ups
    Pp <- phi %*% Pf %*% t(phi) + Q
    sig1 <- as.numeric(M1 %*% Pp %*% t(M1) + R)
    sig2 <- as.numeric(M2 %*% Pp %*% t(M2) + R)
    k1 <- Pp %*% t(M1) / sig1
    k2 <- Pp %*% t(M2) / sig2
    e1 <- y[i] - M1 %*% xp
    e2 <- y[i] - M2 %*% xp
    
    # Updating the probability for two possible states
    pip1 <- pif1 * pi11 + pif2 * pi21
    pip2 <- pif1 * pi12 + pif2 * pi22
    
    den1 <- (1 / sqrt(sig1)) * exp(-0.5 * e1 ^ 2 / sig1)
    den2 <- (1 / sqrt(sig2)) * exp(-0.5 * e2 ^ 2 / sig2)
    
    denm <- pip1 * den1 + pip2 * den2
    
    pif1 <- as.numeric(pip1 * den1 / denm)
    pif2 <- as.numeric(pip2 * den2 / denm)
    
    e1 <- as.numeric(e1)
    e2 <- as.numeric(e2)
    
    xf <- xp + pif1 * k1 * e1 + pif2 * k2 * e2
    
    eye <- diag(1, nstate)
    
    Pf <- pif1 * (eye - k1 %*% M1) %*% Pp + pif2 * (eye - k2 %*% M2) %*% Pp
    
    like <- like - log(pip1 * den1 + pip2 * den2)
    
    prob[i] <<- pip2
    
    xfilter[,,i] <<- xf
    
    innov.sig <<- c(sig1, sig2)
    
    yp[i] <<- ifelse(pip1 > pip2, M1 %*% xp, M2 %*% xp)
    
  }
    
    return(like)
}



# ----------
# Estimation

alpha1 <- 1.4;  alpha2 <- -0.5;  beta0 <- 0.3;  sQ1 <- 0.1;  sQ2 <- 0.1;

init.par <- c(alpha1, alpha2, beta0, sQ1, sQ2)

( est <- optim(init.par, Linn, NULL, method = "BFGS", hessian = TRUE, control = list(trace = 1, REPORT = 1)))

SE <- sqrt(diag(solve(est$hessian)))



# ----------
u <- cbind(estimate = est$par, SE)

rownames(u) <- c("alpha1", "alpha2", "beta0", "sQ1", "sQ2")

round(u, digits = 4)


# -->
# The estimated level shift (beta0) corresponds to an increase in mortality by about 0.2 per 1000 during a flue epidemic



# ----------
# Estimated standard prediction error
# When no epidemic is predicted:  0.02
# When epidemic is predicted: 0.11

sqrt(innov.sig)




# ------------------------------------------------------------------------------
# plot the result
# ------------------------------------------------------------------------------

predepi <- ifelse(prob < 0.5, 0, 1)

k <- 6:length(y)

Time <- time(flu)[k]

regime <- predepi[k] + 1


graphics.off()
par(mfrow = c(3, 1), mar = c(2,3,1,1) + 0.1)



# ----------
# original data and a prediction indicator (1 or 2) that an epidemic occurs in month t
# given the data up to month t - 1 (dashed line)
plot(Time, y[k], type = "n", ylab = "")
grid(lty = 2)
lines(Time, y[k], col = gray(0.7))
text(Time, y[k], col = regime, labels = regime, cex = 1.1)
text(1979, 0.80, "(a)")



# ----------
# The three filtered structural components of influenza mortality; cyclic trace, spiked trace, and negative linear trace
plot(Time, xfilter[1,,k], type = "n", ylim = c(-0.1, 0.4), ylab = "")
grid(lty = 2)
lines(Time, xfilter[1,,k])
lines(Time, xfilter[3,,k])
lines(Time, xfilter[4,,k])
text(1979, 0.35, "(b)")



# ----------
# One-month-ahead predictions shown as upper and lower limits (gray swatch)
# of the number of pneumonia and influenza deaths and original data

plot(Time, y[k], type = "n", ylim = c(0.1, 0.9), ylab = "")
grid(lty = 2)
points(Time, y[k], pch = 19)

prde1 <- 2 * sqrt(innov.sig[1])
prde2 <- 2 * sqrt(innov.sig[2])
prde <- ifelse(predepi[k] < 0.5, prde1, prde2)
xx <- c(Time, rev(Time))
yy <- c(yp[k] - prde, rev(yp[k] + prde))
polygon(xx, yy, border = 8, col = gray(0.6, alpha = 0.3))
text(1979, 0.85, "(c)")



