setwd("//media//kswada//MyFiles//R//globtemp")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  globtemp
# ------------------------------------------------------------------------------

data(globtemp, package = "astsa")

data(globtempl, package = "astsa")


str(globtemp)

str(globtempl)





# ------------------------------------------------------------------------------
# Plot two similar time series data
# ------------------------------------------------------------------------------

# The second series, globtempl, are the surface air temperature index data using only meteorological station data.
# Conceptually, both series should be measuring the same underlying climatic signal, and we may consider
# the problem of extracting this underlying signal.

ts.plot(globtemp, globtempl, col = c(4, 3), ylab = "Temeperature Deviations", lwd = 2)




# ------------------------------------------------------------------------------
# Estimate smoothed value from two similar time series by state-space model
#   - We suppose both series are observing the same signal with different noises
#
# Newton-Raphson procedure to update parameter values until the negative of the log likelihood is minimized
#   - We assume that both series should be measuring the same underlying climatic signal, which we model as a random walk with drift
#     x(t) = drift + x(t-1) + w(t)
# ------------------------------------------------------------------------------


y <- cbind(globtemp, globtempl)


num <- nrow(y)


# matrix or vector of inputs having the same row dimension as y
input <- rep(1, num)



( A <- array(rep(1, 2), dim = c(2, 1, num)) )




# ----------
# We hold the initial state parameters fixed in this example,
# which is large relative to the data.

mu0 <- -0.35

Sigma0 <- 1

Phi <- 1




# ------------------------------------------------------------------------------
# Function to calculate likelihood
# ------------------------------------------------------------------------------


# Kfilter1:  model may be time varing or have inputs


Linn <- function(para){
  
  # sigma_w
  cQ <- para[1]

  # 11,22,12 element of chol(R)
  cR1 <- para[2]
  cR2 <- para[3]
  cR12 <- para[4]
  
  # put the matrix together
  cR <- matrix(c(cR1, 0, cR12, cR2), 2)
  
  drift <- para[5]
  
  kf <- astsa::Kfilter1(num, y, A, mu0, Sigma0, Phi, drift, 0, cQ, cR, input)
  
  return(kf$like)
}




# ------------------------------------------------------------------------------
# Estimation
# ------------------------------------------------------------------------------

# initial values of parameters
init.par <- c(0.1, 0.1, 0.1, 0, 0.05)


( est <- optim(init.par, Linn, NULL, method = "BFGS", hessian = TRUE, control = list(trace = 1, REPORT = 1)) )


est$hessian


SE <- sqrt(diag(solve(est$hessian)))



# ----------
u <- cbind(estimate = est$par, SE)

rownames(u) <- c("sigw", "CR11", "CR22", "CR12", "drift")

u




# ------------------------------------------------------------------------------
# smoothing
# ------------------------------------------------------------------------------


cQ <- est$par[1];  cR1 <- est$par[2];  cR2 <- est$par[3];  cR12 <- est$par[4];  drift <- est$par[5]


cR <- matrix(c(cR1, 0, cR12, cR2), 2)


# estimated R matrix
( R <- t(cR) %*% cR )



ks <- astsa::Ksmooth1(num, y, A, mu0, Sigma0, Phi, drift, 0, cQ, cR, input)



# ----------
xsm <- ts(as.vector(ks$xs), start = 1880)

rmse <- ts(sqrt(as.vector(ks$Ps)), start = 1880)

xx <- c(time(xsm), rev(time(xsm)))

yy <- c(xsm - 2 * rmse,  rev(xsm + 2 * rmse))





# ------------------------------------------------------------------------------
# Display estimates
# ------------------------------------------------------------------------------


graphics.off()
par(mfrow=c(1,1))

plot(xsm, ylim = c(-0.6, 1), ylab = "Temperature Deviations")

polygon(xx, yy, border = NA, col = gray(0.6, alpha = 0.25))

lines(globtemp, type = "o", pch = 4, col = 4, lty = 6)

lines(globtempl, type = "o", pch = 3, col = 3, lty = 6)



# -->
# The dashed lines with points are the two average global temperature deviations.
# The solid line is the estimated smoother and the corresponding two root mean square error bound is the gray swatch.


