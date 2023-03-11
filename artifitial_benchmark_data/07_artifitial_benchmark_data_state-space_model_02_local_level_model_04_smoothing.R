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
# Smoothing
# ------------------------------------------------------------------------------

oSmoothed.DLM <- dlmSmooth(oFiltered.DLM)



# ----------
# average of smoothed distribution

oSmoothed.DLM$s

cat(sprintf("hat(mu_1) = %f\n", oSmoothed.DLM$s[2]))




# ------------------------------------------------------------------------------
# smoothed series and its 95% confidence interval
# ------------------------------------------------------------------------------

var <- dlmSvd2var(oSmoothed.DLM$U.S, oSmoothed.DLM$D.S)

hwid <- qnorm(0.025, lower = FALSE) * sqrt(unlist(var))

smooth <- cbind(oSmoothed.DLM$s, as.vector(oSmoothed.DLM$s) + hwid %o% c(-1, 1))



# ----------
par(mfrow = c(1,1))
plot(dropFirst(smooth), plot.type = "s", type = "l", lty = c(1,5,5), xlab = "", ylab = "Level", ylim = range(y))

lines(y, type = "o", col = "darkgray")



# ------------------------------------------------------------------------------
# Original series, Filtered series, and Smoothed series
# ------------------------------------------------------------------------------


plot(y, type = "o", col = "darkgray")

lines(dropFirst(oFiltered.DLM$m), lty = 1, col = "black", lwd = 2)

lines(dropFirst(oSmoothed.DLM$s), lty = 2, col = "blue", lwd = 2)

# f:  average of one-step-ahead prediction distribution = F %:% a
lines(oFiltered.DLM$f, lty = 3, col = "orange", lwd = 2)




