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
# State-Space model by KFAS:  local level model (random walk + noize)
# ------------------------------------------------------------------------------

# 1:  random walk plus noise
# H:  variance of observed noise
# Q:  variance of system noise

( oModel.KFAS <- SSModel(Nile ~ SSMtrend(1, Q = list(matrix(NA))), H = matrix(NA)) )



# ----------
# fitSSM:  estimation by maximum likelihood
# Need to set some initial value (not zero) and log is required to avoid overflow

( oFitted.KFAS <- fitSSM(oModel.KFAS, init = c(log(var(Nile)), log(var(Nile))), method = "BFGS") )



# ----------
# check the convergence = 0
oFitted.KFAS$optim.out



# ----------
drop(oFitted.KFAS$model$H)

drop(oFitted.KFAS$model$Q)



# ----------
# the noise ratio W/V
drop(oFitted.KFAS$model$Q) / drop(oFitted.KFAS$model$H)




# ------------------------------------------------------------------------------
# Filtered and Smoothed series by KFAS
# ------------------------------------------------------------------------------


# filtering:  default is "state", Gauss model
# smoothing:  default is "state" and "mean" ("signal")

oEstimated.KFAS <- KFS(oFitted.KFAS$model, filtering = "state", smoothing = "state")


names(oEstimated.KFAS)




# ----------
par(mfrow = c(1,1))
ts.plot(Nile, type = "o", col = c("darkgray"))


# state one-step-ahead prediction
lines(dropFirst(oEstimated.KFAS$a), col = "black")


# state smoothed
lines(dropFirst(oEstimated.KFAS$alphahat), col = "blue")



# ------------------------------------------------------------------------------
# Model diagnostics
# ------------------------------------------------------------------------------


plot(oFitted.KFAS$model)



# ----------
( oEstimated2.KFAS <- KFS(oFitted.KFAS$model, smoothing = c("state", "mean", "disturbance")) )


plot(cbind(state = rstandard(oEstimated2.KFAS, "state"), recursive = rstandard(oEstimated2.KFAS), 
           irregular = rstandard(oEstimated2.KFAS, "pearson")), main = "recursive and auxiliary residuals")




# ------------------------------------------------------------------------------
# Model diagnostics:  Independence / Homoscedasticity / Normality
# ------------------------------------------------------------------------------

agStdPredErr.KFAS <- oEstimated.KFAS$v[,1] / sqrt(t(oEstimated.KFAS$F))


sub.ShowDiagnostics(agStdPredErr.KFAS, nStateVar, nHyperParam, nMaxLag = 15, anACLag = c(1, 12))




# ------------------------------------------------------------------------------
# Goodness of Fit
# ------------------------------------------------------------------------------

# model's log-likelihood and AIC

( gLLbyFun.KFAS <- oEstimated.KFAS$logLik )

sub.AIC(gLLbyFun.KFAS, nStateVar, nHyperParam)

sub.AIC(gLLbyFun.KFAS, nStateVar, nHyperParam) / n



# ----------
# v[,1]:  prediction errors
# F:  variance of prediction errors

( gLLbyErr.KFAS <- sub.LogLik(oEstimated.KFAS$v[,1], t(oEstimated.KFAS$F), nStateVar) )

sub.AIC(gLLbyErr.KFAS, nStateVar, nHyperParam)

sub.AIC(gLLbyErr.KFAS, nStateVar, nHyperParam) / n




# ------------------------------------------------------------------------------
# Smoothed level and 95% confidence interval
# ------------------------------------------------------------------------------

( conf <- predict(oFitted.KFAS$model, interval = "confidence", level = 0.95) )

( pred <- predict(oFitted.KFAS$model, interval = "prediction", level = 0.95) )


ts.plot(cbind(Nile, pred, conf[,-1]), col = c(1:2, 3, 3 ,4 ,4), ylab = "Predicted Annual flow", main = "River Nile")




# ------------------------------------------------------------------------------
# Future Focecast
# ------------------------------------------------------------------------------

oModel2.KFAS <- SSModel(Nile ~ SSMtrend(1, Q = list(oFitted.KFAS$model$Q)), H = oFitted.KFAS$model$H)


( oFcst.KFAS <- predict(oModel2.KFAS, n.ahead = 10, interval = "prediction", level = 0.9) )


ts.plot(oFcst.KFAS, col = c(1,2,3))



