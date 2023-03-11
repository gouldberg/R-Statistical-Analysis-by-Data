setwd("//media//kswada//MyFiles//R//jj")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Johnson & Johnson Quarterly Earnings
# ------------------------------------------------------------------------------

data(jj, package = "astsa")

str(jj)

jj


# ----------
x <- jj

lx <- log(x)

dlx <- diff(lx)

ddlx <- diff(dlx, 4)



# ------------------------------------------------------------------------------
# Signal Extraction (trend, seasonal and irregular components)
# ------------------------------------------------------------------------------

num <- length(jj)


A <- cbind(1, 1, 0, 0)



# ----------
# Function to calculatge likelihood

Linn <- function(para){
  
  Phi <- diag(0, 4)
  
  # Trend compoenet:  Trend to increase exponentially by Phi[1,1] (= growth rate)
  # T(t) = Phi[1,1] * T(t-1) + w(t1)
  Phi[1,1] <- para[1]
  
  Phi[2,] <- c(0, -1, -1, -1)
  
  Phi[3,] <- c(0, 1, 0, 0)
  
  Phi[4,] <- c(0, 0, 1, 0)
  
  cQ1 <- para[2]
  
  cQ2 <- para[3]
  
  cQ <- diag(0, 4)
  
  cQ[1,1] <- cQ1
  
  cQ[2,2] <- cQ2
  
  cR <- para[4]
  
  kf <- Kfilter0(num, jj, A, mu0, Sigma0, Phi, cQ, cR)
  
  return(kf$like)

}



# ----------
# Initial parameters

mu0 <- c(0.7, 0, 0, 0)

Sigma0 <- diag(0.04, 4)


# Phi[1,1], the 2 cQs and cR
# Grawth rate (= Phi[1,1]) is about 3% per year
init.par <- c(1.03, 0.1, 0.1, 0.5)



# ----------
# Estimation and Results

( est <- optim(init.par, Linn, NULL, method = "BFGS", hessian = TRUE, control = list(trace = 1, REPORT = 1)) )


SE <- sqrt(diag(solve(est$hessian)))


u <- cbind(estimate = est$par, SE)


rownames(u) <- c("Phi11", "sigw1", "sigw2", "sigv")


u



# ----------
# Smooth

Phi <- diag(0, 4)

Phi[1,1] <- est$par[1]

Phi[2,] <- c(0, -1, -1, -1)

Phi[3,] <- c(0, 1, 0, 0)

Phi[4,] <- c(0, 0, 1, 0)

cQ1 <- est$par[2]

cQ2 <- est$par[3]

cQ <- diag(1, 4)

cQ[1,1] <- cQ1

cQ[2,2] <- cQ2

cR <- est$par[4]

ks <- Ksmooth0(num, jj, A, mu0, Sigma0, Phi, cQ, cR)


names(ks)



# ----------
# Plots

Tsm <- ts(ks$xs[1,,], start = 1960, freq = 4)

Ssm <- ts(ks$xs[2,,], start = 1960, freq = 4)

p1 <- 3 * sqrt(ks$Ps[1,1,])

p2 <- 3 * sqrt(ks$Ps[2,2,])



par(mfrow = c(2,1))

plot(Tsm, main = "Trend Component", ylab = "Trend")
xx <- c(time(jj), rev(time(jj)))
yy <- c(Tsm - p1, rev(Tsm + p1))
polygon(xx, yy, border = NA, col = gray(0.5, alpha = 0.3))


plot(jj, main = "Data & Trend + Season", ylab = "J&J QW/Share", ylim = c(-0.5, 17))
xx <- c(time(jj), rev(time(jj)))
yy <- c(Tsm+Ssm - p1 - p2, rev(Tsm + Ssm + p1 + p2))
polygon(xx, yy, border = NA, col = gray(0.5, alpha = 0.3))
