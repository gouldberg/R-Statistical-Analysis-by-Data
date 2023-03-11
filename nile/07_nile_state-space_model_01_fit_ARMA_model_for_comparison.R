rm(list=ls())

packages <- c("dplyr", "dlm", "KFAS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Nile
# ------------------------------------------------------------------------------

data(Nile)


dim(Nile)


str(Nile)




# ------------------------------------------------------------------------------
# Fit ARIMA(1,0,3) model
# ------------------------------------------------------------------------------


result <- arima(Nile, order = c(1,0,3), transform.pars = FALSE)


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
t <- numeric(4)

for(j in 1:4) t[j] <- b[j] / sqrt(V[j,j])


names(t) <- c("t_a1", "t_ma1", "t_ma2", "t_ma3")

t


hantei <- ( (t < 0) & (pnorm(t) < 0.05) ) | ( (t > 0) & (pnorm(t) > 0.95) )


hantei



# ------------------------------------------------------------------------------
# Prediction
# ------------------------------------------------------------------------------


ahat <- result$residuals

result_hat <- Nile - ahat


plot(Nile, type = "o", col = c("darkgrey"))

lines(result_hat, lty = 2, col = 2)



# ----------
# n.ahead = 10

( pred <- predict(result, n.ahead = 10) )


se1 <- pred$pred + 2 * pred$se

se2 <- pred$pred - 2 * pred$se


ts.plot(result_hat, pred$pred, se1, se2, gpars = list(lt = c(2,3,4), col = c(2,3,4), ylim = range(Nile)))
lines(Nile, type = "o", col = c("darkgrey"))


