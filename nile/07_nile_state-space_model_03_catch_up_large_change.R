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
# Smoothing
# ------------------------------------------------------------------------------

X <- ts(matrix(oFitted.DLM$W, nc = 1, nr = length(Nile)), start = start(Nile))

window(X, 1898, 1899) <- 12 * oFitted.DLM$W


plot(X)



# ----------
oFitted4.DLM <- oFitted.DLM

oFitted4.DLM$X <- X

oFitted4.DLM$JM <- matrix(1,1,1)



# ----------
oFiltered4.DLM <- dlmFilter(Nile, oFitted4.DLM)



# ----------
a <- window(cbind(Nile, oFiltered.DLM$f, oFiltered.DLM$f), start = 1871, end = 1920)

plot(a[,1], type = "o", col = "darkgray", xlab = "", ylab = "Level", main = "DLM catch-up large change")

lines(a[,2], lty = "longdash")

lines(a[,3], lty = "dotdash")

abline(v = 1898, lty = 2)


