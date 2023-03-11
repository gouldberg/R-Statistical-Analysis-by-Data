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
# Original time series + 1st difference (high-pass filter) and 12 months filter (low-pass filter)
#   - differencing:  example of a high-pass filter because it retains or passes the higher frequencies
#   - moving average:  low-pass filter because it passes the lower or slower frequencies
# ------------------------------------------------------------------------------

par(mfrow=c(3,1), mar=c(3,3,1,1), mgp=c(1.6, 0.6, 0))

plot(y, main = "y")
abline(v = seq(0, 100, by = 10), lty = 2, col = "gray")

plot(diff(y), main = "First Difference")
abline(v = seq(0, 100, by = 10), lty = 2, col = "gray")



# ----------
( k <- kernel("modified.daniell", 3) )

( yf <- kernapply(y, k) )

plot(yf, main = "filter")
abline(v = seq(0, 100, by = 10), lty = 2, col = "gray")





# ------------------------------------------------------------------------------
# Spectrum analysis of the low-pass filtered series
# ------------------------------------------------------------------------------

spectrum(yf, spans = 3, log = "no")


