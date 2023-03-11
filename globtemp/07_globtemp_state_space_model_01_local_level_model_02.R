setwd("//media//kswada//MyFiles//R//globtemp")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  globtemp
# ------------------------------------------------------------------------------

data(globtemp, package = "astsa")


str(globtemp)




# ------------------------------------------------------------------------------
# Function to evaluate the likelihood and estimation
# ------------------------------------------------------------------------------

# A: time-invariant observation matrix
# mu0: initial state mean vector
# Sigma0:  initial state covariance matrix
# Phi:  state transition matrix
# cQ:  Cholesky-type decomposition of state error covariance matrix Q
# cR:  Cholesky-type decomposition of observation error covariance matrix R


Linn <- function(para){
  
  sigw <- para[1]
  
  sigv <- para[2]
  
  cQ <- sigw
  
  kf <- astsa::Kfilter0(length(globtemp), globtemp, A = 1, mu0 = 0, Sigma0 = 1, Phi = 1, cQ = cQ, sigv)
  
  return(kf$like)
}



# ----------
# Estimation

init.par <- c(0.1, 1)

( est <- optim(init.par, Linn, NULL, method = "BFGS", hessian = TRUE, control = list(trace = 1, REPORT = 1)) )


SE <- sqrt(diag(solve(est$hessian)))




# ----------
# Summary of estimation

estimate <- est$par

u <- cbind(estimate, SE)


rownames(u) <- c("sigw", "sigv")

u



# ------------------------------------------------------------------------------
# Smoothing
# ------------------------------------------------------------------------------


y <- globtemp

num <- length(globtemp)



sigw <- est$par[1]


cQ <- diag(c(sigw, 0))


sigv <- est$par[2]

ks <- astsa::Ksmooth0(num, y, A, mu0, Sigma0, Phi, cQ, sigv)



# ----------
# smoothing parameter lambda = sigv^2 / sigw^2
sigv^2 / sigw^2




# -----------
xsmoo <- ts(ks$xs[1,1,])

psmoo <- ts(ks$Ps[1,1,])

upp <- xsmoo + 2 * sqrt(psmoo)

low <- xsmoo - 2 * sqrt(psmoo)



# ----------
graphics.off()

Time <- 1:num


# observations
plot(y, type = "o", col = 8)


plot(Time, y, main = "Precdict, Filter, Smooth", ylim = c(-0.6, 1))
lines(ks$xp, col = "blue", lwd = 2)
# lines(ks$xp + 2 * sqrt(ks$Pp), lty = 2, col = 4)
# lines(ks$xp - 2 * sqrt(ks$Pp), lty = 2, col = 4)

lines(ks$xf, col = "red", lwd = 2, lty = 2)
# lines(ks$xf + 2 * sqrt(ks$Pf), lty = 2, col = 4)
# lines(ks$xf - 2 * sqrt(ks$Pf), lty = 2, col = 4)

lines(ks$xs, col = 3, lwd = 1, lty = 2)
# lines(ks$xs + 2 * sqrt(ks$Ps), lty = 2, col = 4)
# lines(ks$xs - 2 * sqrt(ks$Ps), lty = 2, col = 4)




# ----------
# for reference:
smooth.spline(y)



