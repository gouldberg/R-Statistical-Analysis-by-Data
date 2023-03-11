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




