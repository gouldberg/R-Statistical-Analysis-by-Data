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
# Impute missing time series
# ------------------------------------------------------------------------------

tsDataNA <- Nile

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


ts.plot(tsDataNA, fit, fit_filt, col = 1:3, lty = 1:3, ylab = "Predicted Annual flow", main = "River Nile")




