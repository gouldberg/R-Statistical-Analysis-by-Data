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
plot(Nile, type = "o", col = "darkgray", xlab = "", ylab = "Level", main = "DLM Future Forecasts", xlim = c(1950, 1980))

invisible(lapply(oFcst.DLM$newObs, function(x) lines(x, col = "lightgray", type = "o", pch = 4)))

lines(oFcst.DLM$f, type = "o", lwd = 1, pch = 4)

abline(v = mean(c(time(oFcst.DLM$f)[1], time(Nile)[length(Nile)])), lty = "dashed")




