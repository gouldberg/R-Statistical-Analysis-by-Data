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


