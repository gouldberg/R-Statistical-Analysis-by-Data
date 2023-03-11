setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\10_time_series_basics\\sales")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  sales
# ------------------------------------------------------------------------------


dat <- read.csv("sales.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)


str(dat)


car::some(dat)



# ----------
colnames(dat) <- c("month", "fabrics", "machinery", "fuel")




# ------------------------------------------------------------------------------
# Fit Structural Time Series (by maximum likelihood)
# Seasonality and AR by KFAS
# ------------------------------------------------------------------------------


library(KFAS)



# ----------
# AR(1)

modAR1 <- SSModel(dat$fabrics ~ 
                    SSMtrend(2, Q = c(list(0), list(NA))) +
                    SSMarima(ar = 0, Q = 0) +
                    SSMseasonal(12, sea.type = "dummy"), H = NA)


updatefn <- function(pars, model){
  model <- SSModel(dat$fabrics ~
                     SSMtrend(2, Q = c(list(0), list(exp(pars[1])))) +
                     SSMarima(ar = artransform(pars[2]), Q = exp(pars[3])) +
                     SSMseasonal(12, sea.type = "dummy"), H = exp(pars[4]))
  return(model)
}



fitAR1 <- fitSSM(modAR1, c(-1, 0, 6, 3), updatefn, method = "BFGS")

kfsAR1 <- KFS(fitAR1$model)




# ----------
# AR(2)

modAR2 <- SSModel(dat$fabrics ~ 
                    SSMtrend(2, Q = c(list(0), list(NA))) +
                    SSMarima(ar = c(0, 0), Q = 0) +
                    SSMseasonal(12, sea.type = "dummy"), H = NA)


updatefn <- function(pars, model){
  model <- SSModel(dat$fabrics ~
                     SSMtrend(2, Q = c(list(0), list(exp(pars[1])))) +
                     SSMarima(ar = artransform(pars[2:3]), Q = exp(pars[4])) +
                     SSMseasonal(12, sea.type = "dummy"), H = exp(pars[5]))
  return(model)
}



fitAR2 <- fitSSM(modAR2, c(-1, 0.1, 0, 6, 3), updatefn, method = "BFGS")

kfsAR2 <- KFS(fitAR2$model)




# ----------
# AR(12)  but other coefficient is zero other than lag12

modAR12 <- SSModel(dat$fabrics ~ 
                    SSMtrend(2, Q = c(list(0), list(NA))) +
                    SSMarima(ar = rep(0, 12), Q = 0) +
                    SSMseasonal(12, sea.type = "dummy"), H = NA)


updatefn <- function(pars, model){
  model <- SSModel(dat$fabrics ~
                     SSMtrend(2, Q = c(list(0), list(exp(pars[1])))) +
                     SSMarima(ar = c(rep(0, 11), artransform(pars[2])), Q = exp(pars[3])) +
                     SSMseasonal(12, sea.type = "dummy"), H = exp(pars[4]))
  return(model)
}



fitAR12 <- fitSSM(modAR12, c(-1, 0.4, 6, 0), updatefn, method = "BFGS")

kfsAR12 <- KFS(fitAR12$model)





# ----------
graphics.off()

par(mfrow = c(1,1))

plot(dat$fabrics, type = "l", lty = 1, ylab = "sales", xaxt = "n", xaxs = "i", col = 1, main = "level", lwd = 3)

axis(side = 1, at = 1+0:11*12,
     labels = c("02/1","03/1","04/1","05/1","06/1","07/1","08/1","09/1","10/1","11/1","12/1","13/1"))

lines(kfsAR1$alphahat[,"level"], col = "gray")

lines(kfsAR2$alphahat[,"level"], col = "darkgreen")

lines(kfsAR12$alphahat[,"level"], col = "red")



# -->
# levels are all the same



plot(dat$fabrics, type = "l", lty = 1, ylab = "sales", xaxt = "n", xaxs = "i", col = 1, lwd = 3, main = "filtered")

axis(side = 1, at = 1+0:11*12,
     labels = c("02/1","03/1","04/1","05/1","06/1","07/1","08/1","09/1","10/1","11/1","12/1","13/1"))

# lines(dat$fabrics - resid(kfsAR1), col = "gray")
lines(kfsAR1$att, col = "gray")

# lines(dat$fabrics - resid(kfsAR2), col = "darkgreen")
lines(kfsAR2$att, col = "darkgreen")

# lines(dat$fabrics - resid(kfsAR12), col = "red")
lines(kfsAR12$att, col = "red")



# ----------
# maximum likelihood

likAR1 <- kfsAR1$logLik - sum(kfsAR1$Finf > 0) * log(2*pi)/2

likAR2 <- kfsAR2$logLik - sum(kfsAR2$Finf > 0) * log(2*pi)/2

likAR12 <- kfsAR12$logLik - sum(kfsAR12$Finf > 0) * log(2*pi)/2



# ----------
# mean square error of one-step-ahead prediction
( mseAR1 <- sum(kfsAR1$v[14:144]^2) / 131 )

( mseAR2 <- sum(kfsAR2$v[14:144]^2) / 131 )

( mseAR12 <- sum(kfsAR12$v[14:144]^2) / 131 )



# -->
# AR12 is the best




# ----------
# modify log likelihood to be compared to fixed seasonality

likSeasTri <- likSeasTri - (likSeasTri0 - likSeasDummy0)

likSeasTri0 <- likSeasDummy0




# ----------
# AIC
-2 * likAR1 + 2 * (4 + 13)

-2 * likAR2 + 2 * (5 + 13)

-2 * likAR12 + 2 * (4 + 13)



# -->
# AR12 is the best




