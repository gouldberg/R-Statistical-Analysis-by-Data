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
# Forecast
# ------------------------------------------------------------------------------


# sampleNew = 3:  predict 3 times

oFcst.DLM <- dlmForecast(oFiltered.DLM, nAhead = 10, sampleNew = 3)


# ----------
# average and variance of distribution for state
oFcst.DLM$a

oFcst.DLM$R



# ----------
# average and variance of distribution for future observation
oFcst.DLM$f

oFcst.DLM$Q



# ----------
oFcst.DLM$newStates


oFcst.DLM$newObs



# ----------
plot(y, type = "o", col = "darkgray", xlab = "", ylab = "Level", main = "DLM Future Forecasts")

invisible(lapply(oFcst.DLM$newObs, function(x) lines(x, col = "lightgray", type = "o", pch = 4)))

lines(oFcst.DLM$f, type = "o", lwd = 1, pch = 4)

abline(v = mean(c(time(oFcst.DLM$f)[1], time(Nile)[length(y)])), lty = "dashed")




