setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\sales")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  sales
# ------------------------------------------------------------------------------


dat <- read.csv("sales.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)


Gasoline <- read.table("Gasoline.dat", header = FALSE, sep = "", stringsAsFactors = FALSE) %>% .[[1]]


str(dat)


car::some(dat)

car::some(Gasoline)



# ----------
colnames(dat) <- c("month", "fabrics", "machinery", "fuel")




# ------------------------------------------------------------------------------
# data exploration:  time series plot
# ------------------------------------------------------------------------------

graphics.off()

par(mfrow = c(2,1))

plot(dat$fuel, type = "b")

plot(Gasoline, type = "b")




# ------------------------------------------------------------------------------
# Fit Structural Time Series (by maximum likelihood)
# Time Varying Coefficient
# ------------------------------------------------------------------------------


library(KFAS)


# ----------
# no regression

modReg0 <- SSModel(log(dat$fuel) ~ SSMtrend(1, Q = NA) + 
                     SSMseasonal(12, Q = 0), H = NA)




# ----------
# fixed coefficient

modReg1 <- SSModel(log(dat$fuel) ~ SSMtrend(1, Q = NA) +
                     SSMseasonal(12, Q = 0) +
                     log(Gasoline), H = NA)


# ----------
# time varying coefficient

modReg2 <- SSModel(log(dat$fuel) ~ SSMtrend(1, Q = NA) +
                           SSMseasonal(12, Q = 0) +
                           SSMregression(~ log(Gasoline), Q = NA), H = NA)



# ----------
# fitReg0 <- fitSSM(modReg0, rep(-8, 2), method = "BFGS")
fitReg0 <- fitSSM(modReg0, numeric(2), method = "BFGS")

# fitReg1 <- fitSSM(modReg1, rep(-8, 2), method = "BFGS")
fitReg1 <- fitSSM(modReg1, numeric(2), method = "BFGS")

# fitReg2 <- fitSSM(modReg2, rep(-8, 3), method = "BFGS")
fitReg2 <- fitSSM(modReg2, numeric(3), method = "BFGS")



# ----------
kfsReg0 <- KFS(fitReg0$model)

kfsReg1 <- KFS(fitReg1$model)

kfsReg2 <- KFS(fitReg2$model)




# ----------
# maximum likelihood
logLikReg0 <- kfsReg0$logLik - sum(kfsReg0$Finf > 0) * log(2*pi)/2

logLikReg1 <- kfsReg1$logLik - sum(kfsReg1$Finf > 0) * log(2*pi)/2

logLikReg2 <- kfsReg2$logLik - sum(kfsReg2$Finf > 0) * log(2*pi)/2



# ----------
# AIC
( AICReg0 <- -2 * logLikReg0 + 2 * ( 2 + 12 ) )

( AICReg1 <- -2 * logLikReg1 + 2 * ( 2 + 13 ) )

( AICReg2 <- -2 * logLikReg2 + 2 * ( 3 + 13 ) )



# ----------
# no difference between mod1 and mod2
kfsReg2$alphahat[,"log(Gasoline)"] - kfsReg1$alphahat[,"log(Gasoline)"]




# ----------
# Smoothed level and coefficient

head(kfsReg2$alphahat, 100)


par(mfrow = c(3,1))

par(mar = c(2,4,1,1))

plot(log(dat$fuel), type = "o", lty = 3, xaxt = "n", xlab = "", ylab = "販売額（対数）")
lines(kfsReg1$alphahat[,"level"] + kfsReg1$alphahat[,"log(Gasoline)"] * log(Gasoline),lwd=2)
axis(side = 1, at = 1+0:11*12, labels = c("02/1","03/1","04/1","05/1","06/1","07/1","08/1","09/1","10/1","11/1","12/1","13/1"))

plot(kfsReg1$alphahat[,"level"], xaxt = "n", xlab = "", ylab = "水準成分")
axis(side = 1, at = 1+0:11*12, labels = c("02/1","03/1","04/1","05/1","06/1","07/1","08/1","09/1","10/1","11/1","12/1","13/1"))

plot(kfsReg1$alphahat[,"log(Gasoline)"] * log(Gasoline), ylim = c(1.4,1.6), xaxt = "n", xlab = "", ylab = "回帰成分")
axis(side = 1, at = 1+0:11*12, labels = c("02/1","03/1","04/1","05/1","06/1","07/1","08/1","09/1","10/1","11/1","12/1","13/1"))



