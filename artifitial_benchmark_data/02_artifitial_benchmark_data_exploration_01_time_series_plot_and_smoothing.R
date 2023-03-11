rm(list=ls())

packages <- c("dplyr", "dlm", "KFAS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Artifitial benchmark data
# ------------------------------------------------------------------------------

# generate benchmark data


set.seed(23)


library(dlm)



# ----------
# set model
W <- 1

V <- 2

m0 <- 10

C0 <- 9


# State Equation
f <- function(x, t) 1 / 2 * x + 25 * x / (1 + x^2) + 8 * cos(1.2 * t)


# Observation
h <- function(x) x^2 / 20


t_max <- 100


x_true <- rep(NA_real_, times = t_max + 1)

y <- rep(NA_real_, times = t_max + 1)


x_true[1] <- m0

for(it in (1:t_max) + 1){
  x_true[it] <- f(x_true[it - 1], it) + rnorm(n = 1, sd = sqrt(W))
  
  y[it] <- h(x_true[it]) + rnorm(n = 1, sd = sqrt(V))
}


x_true <- x_true[-1]

y <- y[-1]



# ----------

dat <- data.frame(x_true = x_true, y = y)

ts.plot(dat, ylab = "y", type = "l", lty = c(1,1), col = c("black", "gray"))


y <- as.ts(y)



# ------------------------------------------------------------------------------
# data exploration:  time series plot
# ------------------------------------------------------------------------------

ts.plot(y)

abline(v = seq(0, 200, by = 10), lty = 2, col = "gray")



# ----------
forecast::ndiffs(y)



# ----------
ts.plot(diff(y))

abline(v = seq(0, 100, by = 10), lty = 2, col = "gray")




# ------------------------------------------------------------------------------
# data exploration:  smoothing splines
# ------------------------------------------------------------------------------

# In each interval, one fits a polynomial regression, typically the order is 3, and this is called cubic splines.
# Also a related method is smoothing splines, which minimizes a compromise between the fit and the degree of smoothness

par(mfrow=c(1,1))

plot(y,)


lines(smooth.spline(time(y), y, spar = 0.25), lwd = 2, col = "red")

lines(smooth.spline(time(y), y, spar = 0.5), lwd = 2, col = "blue")

lines(smooth.spline(time(y), y, spar = 1), lty = 2, lwd = 2, col = "black")

abline(v = seq(0, 100, by = 10), lty = 2, col = "gray")




# -----------
par(mfrow=c(1,1))

plot(diff(y))


lines(smooth.spline(time(diff(y)), diff(y), spar = 0.25), lwd = 2, col = "red")

lines(smooth.spline(time(diff(y)), diff(y), spar = 0.5), lwd = 2, col = "blue")

lines(smooth.spline(time(diff(y)), diff(y), spar = 1), lty = 2, lwd = 2, col = "black")

abline(v = seq(0, 100, by = 10), lty = 2, col = "gray")





