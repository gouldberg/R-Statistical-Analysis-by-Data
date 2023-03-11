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
# Fit ARIMA model
# ------------------------------------------------------------------------------


result <- arima(y, order = c(1,0,3), transform.pars = FALSE)


# ----------
summary(result)




# ------------------------------------------------------------------------------
# Model diagnostics
# ------------------------------------------------------------------------------

tsdiag(result)




# ------------------------------------------------------------------------------
# parameter's t statistics
# ------------------------------------------------------------------------------


( b <- result$coef )


# variance of estimated parameters (sqrt of diagonal elements are standard errors)
( V <- result$var.coef )



# ----------
t <- numeric(5)

for(j in 1:5) t[j] <- b[j] / sqrt(V[j,j])


# names(t) <- c("t_a1", "t_ma1")

t


hantei <- ( (t < 0) & (pnorm(t) < 0.05) ) | ( (t > 0) & (pnorm(t) > 0.95) )


hantei



# ------------------------------------------------------------------------------
# Prediction
# ------------------------------------------------------------------------------

ahat <- result$residuals

result_hat <- y - ahat


par(mfrow = c(1,1))

plot(y, type = "o", col = c("darkgrey"))

lines(result_hat, lty = 1, col = 2, lwd = 2)



# ----------
# n.ahead = 10

( pred <- predict(result, n.ahead = 10) )


se1 <- pred$pred + 2 * pred$se
se2 <- pred$pred - 2 * pred$se


ts.plot(result_hat, pred$pred, se1, se2, gpars = list(lt = c(2,3,4), col = c(2,3,4), ylim = range(y)))
lines(y, type = "o", col = c("darkgrey"))


