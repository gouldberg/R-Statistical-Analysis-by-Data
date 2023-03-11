rm(list=ls())

setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\nile")

packages <- c("dplyr", "dlm", "KFAS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Nile
# ------------------------------------------------------------------------------

data(Nile)


dim(Nile)


str(Nile)



# ----------
y <- Nile


t_max <- length(y)



# ------------------------------------------------------------------------------
# FUNCTION:  local-level model with change point (variance of W is time-varying)
# ------------------------------------------------------------------------------

build_dlm <- function(par){
  
  tmp <- dlmModPoly(order = 1, dV = exp(par[1]))
  
  tmp$JW <- matrix(1, nrow = 1, ncol = 1)
  
  tmp$X <- matrix(exp(par[2]), nrow = t_max, ncol = 1)
  
  j <- which(time(y) == 1899)
  
  tmp$X[j, 1] <- tmp$X[j, 1] * exp(par[3])
  
  return(tmp)
}



# ------------------------------------------------------------------------------
# Estimate by MLE
# ------------------------------------------------------------------------------


fit_dlm <- dlmMLE(y = y, parm = rep(0, 3), build = build_dlm)

modtv <- build_dlm(fit_dlm$par)



# ----------
as.vector(modtv$X)


modtv$V




# ------------------------------------------------------------------------------
# Kalman smoothing
# ------------------------------------------------------------------------------

# Kalman smoothing
dlmSmoothed_obj <- dlmSmooth(y = y, mod = modtv)



# average of smoothed distribution
stv <- dropFirst(dlmSmoothed_obj$s)



# ------------------------------------------------------------------------------
# plot
# ------------------------------------------------------------------------------

ts.plot(cbind(y, stv), lty = c("solid", "solid"), col = c("lightgray", "black"), lwd = 2)


legend(legend = c("観測値", "平均 （時不変カルマン平滑化)"),
       lty = c("solid", "solid"), lwd = c(2,2),
       col = c("lightgray", "black"),
       x = "topright", cex = 0.6)




