setwd("//media//kswada//MyFiles//R//cmort")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# data:  cmort (Pollution, Temperature and Mortality)
# ------------------------------------------------------------------------------

data(cmort, package = "astsa")
data(tempr, package = "astsa")
data(part, package = "astsa")


str(cmort)
str(tempr)
str(part)


cmort
tempr
part




# ------------------------------------------------------------------------------
# Use Newton-Raphson and the Kalman filter to fit all the parameters simultanesouly
# ------------------------------------------------------------------------------


# center time  -->  alf is close to mean(cmort)
trend <- time(cmort) - mean(time(cmort))


# appropriate time series of 1s
const <- time(cmort) / time(cmort)



# ----------
# P and P4 are highly correlated, so orthogonalizing these two inputs would be advantageous
# although we did not do it here.

ded <- ts.intersect(M = cmort, T1 = stats::lag(tempr, -1), P = part, P4 = stats::lag(part, -4), trend, const)



y <- ded[,1]

input <- ded[,2:6]

num <- length(y)




# ----------
# Time-varying observatioin matrix an array with dim = c(q, p, n)

( A <- array(c(1, 0), dim = c(1, 2, num)) )




# ----------
# Function to calculate likelihood
# Kfilter2: Kalman Filter model may be time varying or have inputs or correlated errors

Linn <- function(para){
  
  phi1 <- para[1];  phi2 <- para[2];  cR = para[3];  b1 = para[4];  b2 <- para[5];  b3 <- para[6];  b4 <- para[7];  alf <- para[8];
  
  # initial state mean
  mu0 <- matrix(c(0, 0), 2, 1)
  
  # initial state covariance matrix
  Sigma0 <- diag(100, 2)
  
  # state transition matrix
  Phi <- matrix(c(phi1, phi2, 1, 0), 2)
  
  # state error pre-matrix
  Theta <- matrix(c(phi1, phi2), 2)
  
  # state input matrix
  Ups <- matrix(c(b1, 0, b2, 0, b3, 0, 0, 0, 0, 0), 2, 5)
  
  # obaservation input matrix
  Gam <- matrix(c(0, 0, 0, b4, alf), 1, 5)

  # Cholesky decomposition of state error covariance matrix  
  cQ <- cR
  
  # covariance-type matrix of state and observation errors
  S <- cR^2
  
  kf <- astsa::Kfilter2(num, y, A, mu0, Sigma0, Phi, Ups, Gam, Theta, cQ, cR, S, input)
  
  return(kf$like)
}




# ------------------------------------------------------------------------------
# Estimation
# ------------------------------------------------------------------------------


summary(mod_lm)


sarima(u[,1], 0, 0, 0, xreg = u[,2:6])


fit1 <- sarima(cmort, 2, 0, 0, xreg = time(cmort))


fit1$ttable




# ----------
# Set referring to previous modeling by lm() or sarima()

init.par <- c(phi1 = 0.3, phi2 = 0.3, cR = 5, b1 = -0.2, b2 = 0.1, b3 = 0.05, b4 = -1.6, alf = mean(cmort))


# Lower bound and upper bound on parameters used in optim
L <- c(0, 0, 1, -1, 0, 0, -2, 70)


U <- c(0.5, 0.5, 10, 0, 0.5, 0.5, 0, 90)



# ----------
( est <- optim(init.par, Linn, NULL, method = "L-BFGS-B", lower = L, upper = U, hessian = TRUE, control = list(trace = 1, REPORT = 1, factr = 10^8)) )


SE <- sqrt(diag(solve(est$hessian)))



# ----------
# results
round(cbind(estimate = est$par, SE), 3)



# -->
# We notice that the value of estimated parameters are close to initial values estimated by lm() and sarima()




