rm(list=ls())

packages <- c("dplyr", "dlm", "KFAS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Artifitial local level model
# ------------------------------------------------------------------------------

# generate local level model data


set.seed(23)


library(dlm)



# ----------
# set model
W <- 1

V <- 2

m0 <- 10

C0 <- 9

mod <- dlmModPoly(order = 1, dW = W, dV = V, m0 = m0, C0 = C0)



# ----------
# generate observation by kalman forecast

t_max <- 200

sim_data <- dlmForecast(mod = mod, nAhead = t_max, sampleNew = 1)

y <- ts(as.vector(sim_data$newObs[[1]]))

plot(y, ylab = "y")



# ------------------------------------------------------------------------------
# Fit ARIMA model
# ------------------------------------------------------------------------------


# result <- arima(y, order = c(2,0,10), transform.pars = FALSE)
result <- arima(y, order = c(1,1,1), transform.pars = FALSE)


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
t <- numeric(13)

for(j in 1:13) t[j] <- b[j] / sqrt(V[j,j])


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


