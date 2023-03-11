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
# Impute missing time series
# ------------------------------------------------------------------------------

tsDataNA <- y

tsDataNA[c(21:30, 61:70)] <- NA

ts.plot(tsDataNA)




# ----------
oModel_NA.KFAS <- SSModel(tsDataNA ~ SSMtrend(1, Q = list(matrix(NA))), H = matrix(NA))

oFitted_NA.KFAS <- fitSSM(oModel_NA.KFAS, init = c(log(var(tsDataNA, na.rm = TRUE)), log(var(tsDataNA, na.rm = TRUE))), method = "BFGS")



# ----------
oFitted_NA.KFAS$model$Q

oFitted_NA.KFAS$model$H



# ----------
oEstimated_NA.KFAS <- KFS(oFitted_NA.KFAS$model, filtering = "mean", smoothing = "mean")

( fit <- fitted(oEstimated_NA.KFAS, filtered = TRUE) )

( fit_filt <- fitted(oEstimated_NA.KFAS) )


ts.plot(tsDataNA, fit, fit_filt, col = 1:3, lty = 1:3)




