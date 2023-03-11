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
# seasonality model by StructTS
# ------------------------------------------------------------------------------



# need to define frequency

( fab2 <- ts(dat$fabrics, start = 1, frequency = 12) )



# ----------
( strsea <- StructTS(fab2, type = "BSM") )


# Kalman Filter used in fittting
strsea$model



# ----------
strsea$model$V

strsea$model$h



# ----------
head(strsea$fitted)


# fitted values
head(fitted(strllm))

head(strllm$fitted[,"level"])



# ----------
graphics.off()

par(mfrow = c(1,1))

ts.plot(fab2, type = "b", ylim = c(650, 1150))

lines(fitted(strsea)[,"level"] + fitted(strsea)[,"sea"], lwd = 2, col = "blue")


# residuals are very small ...
resid(strsea)

tsdiag(strsea)




# ------------------------------------------------------------------------------
# Fit Structural Time Series (by maximum likelihood)
# Seasonality by KFAS
# ------------------------------------------------------------------------------


library(KFAS)



# ----------
# Dummy type (fixed) Seasonality
modSeasDummy0 <- SSModel(dat$fabrics ~ SSMtrend(2, Q = c(list(0), list(NA))) + SSMseasonal(12, sea.type = "dummy"), H = NA)


# initial value = numeric(2) = c(0, 0)
fitSeasDummy0 <- fitSSM(modSeasDummy0, numeric(2))

kfsSeasDummy0 <- KFS(fitSeasDummy0$model)




# ----------
# Dummy type, seasonality change

modSeasDummy <- SSModel(dat$fabrics ~ SSMtrend(2, Q = c(list(0), list(NA))) + SSMseasonal(12, sea.type = "dummy", Q = NA), H = NA)

fitSeasDummy <- fitSSM(modSeasDummy, numeric(3))

kfsSeasDummy <- KFS(fitSeasDummy$model)



# ----------
# trigonometric type (fixed)

modSeasTri0 <- SSModel(dat$fabrics ~ SSMtrend(2, Q = c(list(0), list(NA))) + SSMseasonal(12, sea.type = "trigonometric"), H = NA)

fitSeasTri0 <- fitSSM(modSeasTri0, numeric(2))

kfsSeasTri0 <- KFS(fitSeasTri0$model)




# ----------
# trigonometric type, seasonality change

modSeasTri <- SSModel(dat$fabrics ~ SSMtrend(2, Q = c(list(0), list(NA))) + SSMseasonal(12, sea.type = "trigonometric", Q = NA), H = NA)

updatefn <- function(pars, model){
  model$H[] <- exp(pars[1])
  
  diag(model$Q[,,1]) <- c(0, exp(pars[2]), rep(exp(pars[3:8]), c(rep(2,5),1)))
  
  return(model)
}

fitSeasTri <- fitSSM(modSeasTri, c(6,0,1,2,0,0,0,0), updatefn, method="BFGS")

kfsSeasTri <- KFS(fitSeasTri$model)




# ----------
graphics.off()

par(mfrow = c(1,1))

plot(dat$fabrics, type = "l", lty = 1, ylab = "sales", xaxt = "n", xaxs = "i", col = 1, main = "level", lwd = 3)

axis(side = 1, at = 1+0:11*12,
     labels = c("02/1","03/1","04/1","05/1","06/1","07/1","08/1","09/1","10/1","11/1","12/1","13/1"))

lines(kfsSeasDummy0$alphahat[,"level"], col = "gray")

lines(kfsSeasDummy$alphahat[,"level"], col = "darkgreen")

lines(kfsSeasTri0$alphahat[,"level"], col = "red")

lines(kfsSeasTri$alphahat[,"level"], col = "blue")



plot(dat$fabrics, type = "l", lty = 1, ylab = "sales", xaxt = "n", xaxs = "i", col = 1, lwd = 3, main = "filtered")

axis(side = 1, at = 1+0:11*12,
     labels = c("02/1","03/1","04/1","05/1","06/1","07/1","08/1","09/1","10/1","11/1","12/1","13/1"))

# lines(dat$fabrics - resid(kfsSeasDummy0), col = "gray")
lines(kfsSeasDummy0$att, col = "gray")

# lines(dat$fabrics - resid(kfsSeasDummy), col = "darkgreen")
lines(kfsSeasDummy$att, col = "darkgreen")

# lines(dat$fabrics - resid(kfsSeasTri0), col = "red")
lines(kfsSeasTri0$att, col = "red")

# lines(dat$fabrics - resid(kfsSeasTri), col = "blue")
lines(kfsSeasTri$att, col = "blue")




# ----------
# maximum likelihood

likSeasDummy0 <- kfsSeasDummy0$logLik - sum(kfsSeasDummy0$Finf>0) * log(2*pi)/2

likSeasDummy <- kfsSeasDummy$logLik - sum(kfsSeasDummy$Finf>0) * log(2*pi)/2

likSeasTri0 <- kfsSeasTri0$logLik - sum(kfsSeasTri0$Finf>0) * log(2*pi)/2

likSeasTri <- kfsSeasTri$logLik - sum(kfsSeasTri$Finf>0) * log(2*pi)/2



# ----------
# mean square error of one-step-ahead prediction
( mseSeasDummy0 <- sum(kfsSeasDummy0$v[14:144]^2) / 131 )

( mseSeasDummy <- sum(kfsSeasDummy$v[14:144]^2) / 131 )

( mseSeasTri0 <- sum(kfsSeasTri0$v[14:144]^2) / 131 )

( mseSeasTri <- sum(kfsSeasTri$v[14:144]^2) / 131 )



# -->
# trigonometric type, seasonality change  is best




# ----------
# modify log likelihood to be compared to fixed seasonality

likSeasTri <- likSeasTri - (likSeasTri0 - likSeasDummy0)

likSeasTri0 <- likSeasDummy0




# ----------
# AIC
-2 * likSeasDummy0 + 2 * (2 + 13)

-2 * likSeasDummy + 2 * (3 + 13)

-2 * likSeasTri0 + 2 * (2 + 13)

-2 * likSeasTri + 2 * (8 + 13)


# -->
# trigonometric type, seasonality change  is best




# ----------
# but AR(1) is applicable to residuals

astsa::acf2(resid(kfsSeasTri))




# ------------------------------------------------------------------------------
# Fit Structural Time Series (by maximum likelihood)
# compare 1st order and 2nd order trend model
# ------------------------------------------------------------------------------


mod1 <- SSModel(c(dat$fabrics[1:120], rep(NA, 24)) ~ SSMtrend(1, Q = NA) + SSMseasonal(12, Q = 0 ), H = NA)
mod2 <- SSModel(c(dat$fabrics[1:120], rep(NA, 24)) ~ SSMtrend(1, Q = NA) + SSMseasonal(12, Q = NA), H = NA)
mod3 <- SSModel(c(dat$fabrics[1:120], rep(NA, 24)) ~ SSMtrend(2, Q = list(0, NA)) + SSMseasonal(12, Q = 0 ), H = NA)
mod4 <- SSModel(c(dat$fabrics[1:120], rep(NA, 24)) ~ SSMtrend(2, Q = list(0, NA)) + SSMseasonal(12, Q = NA), H = NA)


fit1 <- fitSSM(mod1, numeric(2), method = "BFGS")
fit2 <- fitSSM(mod2, numeric(3), method = "BFGS")
fit3 <- fitSSM(mod3, numeric(2), method = "BFGS")
fit4 <- fitSSM(mod4, numeric(3), method = "BFGS")


kfs1 <- KFS(fit1$model)
kfs2 <- KFS(fit2$model)
kfs3 <- KFS(fit3$model)
kfs4 <- KFS(fit4$model) 


logLik1 <- kfs1$logLik - sum(kfs1$Finf>0) * log(2*pi)/2
logLik2 <- kfs2$logLik - sum(kfs2$Finf>0) * log(2*pi)/2
logLik3 <- kfs3$logLik - sum(kfs3$Finf>0) * log(2*pi)/2
logLik4 <- kfs4$logLik - sum(kfs4$Finf>0) * log(2*pi)/2


-2 * logLik1 + 2 * ( 2 + 12 )
-2 * logLik2 + 2 * ( 3 + 12 )
-2 * logLik3 + 2 * ( 2 + 13 )
-2 * logLik4 + 2 * ( 8 + 13 )


# -->
# 2nd order trend model is not better than 1st order trend model



# ----------
graphics.off()

par(mfrow = c(3,1), ps = 16, mar = c(3.5, 3.5, 1, 1), mgp = c(2, 0.5, 0))

plot(dat$fabrics, type = "l", lty = 1, ylab = "販売額（10億円）", xaxt = "n",xaxs = "i",col = 1,xlab = "(a) 水準成分")
axis(side = 1,at = 1+0:11*12, labels = c("02/1","03/1","04/1","05/1","06/1","07/1","08/1","09/1","10/1","11/1","12/1","13/1"))
lines(kfs2$alphahat[,"level"], col = 3)
lines(kfs4$alphahat[,"level"], col = 4)
abline(v = 120.5, lty = 3)

plot(kfs2$alphahat[,"sea_dummy1"],type = "l", ylab = "販売額（10億円）",xaxt = "n", xaxs = "i", yaxs = "i", col = 3,xlab = "(b) 季節成分")
lines(kfs4$alphahat[,"sea_dummy1"],col = 4)
axis(side = 1,at = 1+0:11*12, labels = c("02/1","03/1","04/1","05/1","06/1","07/1","08/1","09/1","10/1","11/1","12/1","13/1"))
abline(v = 120.5, lty = 3)

plot(dat$fabrics-kfs2$muhat,type = "l", ylab = "販売額（10億円）",　xaxt = "n", xaxs = "i", yaxs = "i", col = 3, ylim = c(-150,150), xlab = "(c) 平滑化観測値撹乱項と長期予測誤差")
lines(dat$fabrics-kfs4$muhat,col = 4)
axis(side = 1,at = 1+0:11*12, labels = c("02/1","03/1","04/1","05/1","06/1","07/1","08/1","09/1","10/1","11/1","12/1","13/1"))
abline(v = 120.5, lty = 3)
abline(h = 0, col = 8)



# -->
# but for long-term prediction, 2nd order trend model is better.

