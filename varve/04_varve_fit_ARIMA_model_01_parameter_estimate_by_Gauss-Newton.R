setwd("//media//kswada//MyFiles//R//varve")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  varve
# ------------------------------------------------------------------------------

data(varve, package = "astsa")

str(varve)



# ------------------------------------------------------------------------------
# Parameter estimate for ARIMA(0,1,1) (logged value for MA(1)) as starting point by Method of Moments
# ------------------------------------------------------------------------------

x <- diff(log(varve))


# rho(1)
( r <- acf(x, lag=1, plot=FALSE)$acf[-1] )


# when abs(rho(1))) < 1/2, the invertible estimate is
( rstart <- (1 - sqrt(1 - 4 * (r ^ 2)))/(2 * r) )



# -->
# rho(1) = -0.397 and theta = -0.495 (as initial estimate)



# ------------------------------------------------------------------------------
# Parameter estimate of ARIMA(0,1,1) model (logged value for MA(1)) for the logarithms of the glacial varve data
#  - Gauss-Newton Estimation
# ------------------------------------------------------------------------------


# For conditional least squares, we approximate the residual sum of squares by conditioning on x and w
# Evaluate Sc on a grid
# calculate the conditional error sum of squares (Sc[p])

c(0) -> w -> z

c() -> Sc -> Sz -> Szw

num <- length(x)

th <- seq(-0.3, -0.94, -0.01)

for(p in 1:length(th)){
  for(i in 2:num){ w[i] <- x[i] - th[p] * w[i-1] }
  Sc[p] <- sum(w^2)
}



# ----------
plot(th, Sc,type = "l", ylab = expression(S[c](theta)), xlab = expression(theta), lwd = 2)



# ----------
# Gauss-Newton Estimation
# Minimizing Sc with respect to beta yields the conditional least squares estimates

c(0) -> w -> z

c() -> Sc -> Sz -> Szw -> para

niter <- 12


# rstart for starting value
para[1] <- rstart

for(p in 1:niter){
  for(i in 2:num){ w[i] <- x[i] - para[p] * w[i-1];  z[i] <- w[i-1] - para[p] * z[i-1] }
  Sc[p] <- sum(w^2)
  Sz[p] <- sum(z^2)
  Szw[p] <- sum(z*w)
  para[p+1] <- para[p] + Szw[p] / Sz[p]
}


round(cbind(iteration = 0:(niter - 1), thetahat = para[1:niter], Sc, Sz), 3)



# ----------
# Conditional sum of squares versus values of the moving average parameter for this example.
# Vertical lines indicate the values of the parameter obtained via Gauss-Newton

par(mfrow=c(1,1))

plot(th, Sc, type="l", ylab = expression(S[c](theta)), xlab = expression(theta), lwd = 2)

abline(v = para[1:niter], lty = 2)

points(para[1:niter], Sc[1:niter], pch = 16)




# ----------
# The final estimate (thetahat) is -0.773
para[niter]



# ----------
# The final estimate of the error variance is 0.236 with 632 degrees of freedom (one is lost in differencing)
Sc[niter] / (num - 1)



# ----------
# estimated standard erro of theta is 0.25
( se <- sqrt( (Sc[niter] / (num - 1)) / Sz[niter] ) )



# ----------
# this leads to a t-value of -30.92 with 632 degrees of freedom.
para[niter] / se



