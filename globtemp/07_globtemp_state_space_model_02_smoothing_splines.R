setwd("//media//kswada//MyFiles//R//globtemp")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  globtemp
# ------------------------------------------------------------------------------

data(globtemp, package = "astsa")


str(globtemp)




# ------------------------------------------------------------------------------
# Second order difference of globtemp
# ------------------------------------------------------------------------------

plot(diff(globtemp, differences = 2), type = "l")




# ------------------------------------------------------------------------------
# for reference fitting by sarima
# ------------------------------------------------------------------------------


astsa::acf2(diff(globtemp, differences = 2))


astsa::sarima(globtemp, p = 3, d = 2, q = 1)



# -->
# sigma^2 = 0.009906  --> sigma = 0.1




# ------------------------------------------------------------------------------
# Estimate parameter
# ------------------------------------------------------------------------------


# 2nd order difference = w(t) and y(t) = m(t) + v(t)

# Phi:
# m(t) = 2 * m(t-1) + w(t)
# m(t-1) = m(t-1)

# A:
# y(t) = m(t) + v(t)



# ----------
# State Space

Phi <- matrix(c(2, 1, -1, 0), 2)

A <- matrix(c(1, 0), 1)

mu0 <- matrix(0, 2)

Sigma0 <- diag(1, 2)



# ----------
Linn <- function(para){
  
  sigw <- para[1]
  
  sigv <- para[2]
  
  cQ <- diag(c(sigw, 0))
  
  kf <- astsa::Kfilter0(num, globtemp, A, mu0, Sigma0, Phi, cQ, sigv)
  
  return(kf$like)
}




# ----------
# Estimation

set.seed(123)

init.par <- c(0.1, 1)

( est <- optim(init.par, Linn, NULL, method = "BFGS", hessian = TRUE, control = list(trace = 1, REPORT = 1)) )

SE <- sqrt(diag(solve(est$hessian)))




# ----------
# Summary of estimation
estimate <- est$par

u <- cbind(estimate, SE)


rownames(u) <- c("sigw", "sigv")

u



# ----------
sigw <- est$par[1]

sigv <- est$par[2]


sigv^2 / sigw^2




# ------------------------------------------------------------------------------
# Smooth
# ------------------------------------------------------------------------------

cQ <- diag(c(sigw, 0))

y <- globtemp

num <- length(globtemp)

ks <- astsa::Ksmooth0(num, y, A, mu0, Sigma0, Phi, cQ, sigv)

xsmoo <- ts(ks$xs[1,1,])

psmoo <- ts(ks$Ps[1,1,])

upp <- xsmoo + 2 * sqrt(psmoo)

low <- xsmoo - 2 * sqrt(psmoo)




# ----------
graphics.off()

par(mfrow = c(1,1))


# observations
plot(c(y), type = "o", col = 8)


# Kalman Smoother, parameter estimated using Newton-Raphson techniques
lines(xsmoo, col = 4, lty = 1, lwd = 3)


lines(upp, col = 4, lty = 2)
lines(low, col = 4, lty = 2)


# Smoothing Splines based on the method of generalized cross-validation (GCV)
lines(smooth.spline(c(y), spar = 0.5), lty = 1, col = "red")


legend("topleft", c("Observations"), pch = c(1), lty = 1, lwd = c(1), col = c(8))
legend("bottomright", c("Smoother", "GCV Spline"), lty = c(1, 1), lwd = c(3, 1), col = c(4, "red"))



# -->
# Kalman smoother and Smoothing splines are very close to each other.



# ----------
# compare same lambda

graphics.off()

par(mfrow = c(1,1))


# observations
plot(c(y), type = "o", col = 8)


# Kalman Smoother, parameter estimated using Newton-Raphson techniques
lines(xsmoo, col = 4, lty = 1, lwd = 3)


# Smoothing Splines based on the method of generalized cross-validation (GCV)
# --> is this corresponding ... ?
lines(smooth.spline(c(y), spar = sqrt(sigw / sigv)), lty = 1, col = "red")

