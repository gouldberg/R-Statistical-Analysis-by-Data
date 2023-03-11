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




