setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\hakusan")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS", "sysid", "control")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Hakusan
# ------------------------------------------------------------------------------


dat <- read.table("HAKUSAN.txt", sep = "", header = T, colClasses = "numeric")


head(dat)



# ----------
# demean

dat$YawRate <- dat$YawRate - mean(dat$YawRate)

dat$Rolling <- dat$Rolling - mean(dat$Rolling)

dat$Pitching <- dat$Pitching - mean(dat$Pitching)

dat$Rudder <- dat$Rudder - mean(dat$Rudder)




# ------------------------------------------------------------------------------
# data preparation
# ------------------------------------------------------------------------------


# convert to idframe
hakusan <- idframe(output = data.frame(dat$YawRate, dat$Rolling, dat$Pitching), input = dat$Rudder, Ts = 1)
hakusan1 <- idframe(output = dat$YawRate, input = dat$Rudder, Ts = 1)
hakusan2 <- idframe(output = dat$Rolling, input = dat$Rudder, Ts = 1)
hakusan3 <- idframe(output = dat$Pitching, input = dat$Rudder, Ts = 1)

colnames(hakusan$output) <- c("YawRate", "Rolling", "Pitching")
colnames(hakusan1$output) <- "YawRate"
colnames(hakusan2$output) <- "Rolling"
colnames(hakusan3$output) <- "Pitching"

colnames(hakusan$input) <- "Rudder"
colnames(hakusan1$input) <- "Rudder"
colnames(hakusan2$input) <- "Rudder"
colnames(hakusan3$input) <- "Rudder"



str(hakusan)


# split data
ze <- dataSlice(hakusan, start = 100, end = 700)
ze1 <- dataSlice(hakusan1, start = 100, end = 700)
ze2 <- dataSlice(hakusan2, start = 100, end = 700)
ze3 <- dataSlice(hakusan3, start = 100, end = 700)

zr <- dataSlice(hakusan, start = 701, end = 1000)
zr1 <- dataSlice(hakusan1, start = 701, end = 1000)
zr2 <- dataSlice(hakusan2, start = 701, end = 1000)
zr3 <- dataSlice(hakusan3, start = 701, end = 1000)



# ----------
# detrending
# default is substracting mean value

# R by default doesn’t allow return of multiple objects.
# The %=% operator and g function in this package facillitate this behaviour

g(ze) %=% detrend(ze)
g(ze1) %=% detrend(ze1)
g(ze2) %=% detrend(ze2)
g(ze3) %=% detrend(ze3)

g(zr) %=% detrend(zr)
g(zr1) %=% detrend(zr1)
g(zr2) %=% detrend(zr2)
g(zr3) %=% detrend(zr3)




# ----------
par(mfrow = c(1,1))

plot(ze)
plot(ze1)
plot(ze2)
plot(ze3)




# ------------------------------------------------------------------------------
# Estimate Frequency Response
# ------------------------------------------------------------------------------


# default lag size of the Hanning window (Default: min (length(x)/10,30))
# frequency points at which the response is evaluated (Default: seq(1,128)/128*pi/Ts --> up to 3.14)
sp <- spa(ze)


# bode plot
#  - Y: amplitude in decibel, phase in degree
#  - X: rad / unit time, 1 means 1 rad/s
plot(sp)




# ------------------------------------------------------------------------------
# Impulse Response Function
# ------------------------------------------------------------------------------


fit1 <- impulseest(ze1, M = 200)
fit2 <- impulseest(ze2, M = 200)
fit3 <- impulseest(ze3, M = 200)


sysid::impulseplot(fit1)
sysid::impulseplot(fit2)
sysid::impulseplot(fit3)





# ----------
# step response function

st1 <- sysid::step(fit1)
st2 <- sysid::step(fit2)
st3 <- sysid::step(fit3)


st$data




# ------------------------------------------------------------------------------
# Estimate ARX model parameter na, nb, nk   Rudder --> YawRate
# ------------------------------------------------------------------------------


( parms <- expand.grid(na = 1:10, nb = 1:10, nk = 0:5) )



parms$aicc <- "NA"
parms$fpe <- "NA"
parms$naic <- "NA"
parms$bic <- "NA"
parms$mse <- "NA"


for(i in 1:nrow(parms)){
  
  na <- parms[i,"na"]
  nb <- parms[i,"nb"]
  nk <- parms[i,"nk"]
  
  e <- try(
    modarx0 <- arx(ze1, order = c(na, nb, nk)),
    silent = TRUE
  )
  if( class(e) == "try-error") {
  }else{
    parms$aicc[i] <- fitch(modarx0)$AICc
    parms$fpe[i] <- fitch(modarx0)$FPE
    parms$naic[i] <- fitch(modarx0)$nAIC
    parms$bic[i] <- fitch(modarx0)$BIC
    parms$mse[i] <- fitch(modarx0)$MSE
  }
}


parms


parms[which.min(parms$aicc),]
parms[which.min(parms$fpe),]
parms[which.min(parms$naic),]
parms[which.min(parms$bic),]
parms[which.min(parms$mse),]





# ----------
# now we try na = 1, nb = 1, nk = 4

modarx <- arx(ze1, order = c(1,1,4), lambda = 0.1)

modarx$sys


unlist(fitch(modarx))




# ----------
# fitted values
pred_arx <- modarx$fitted.values[,1]

output <- c(ze1$output)


par(mfrow = c(1,1))

plot(output, type = "l")

lines(time(pred_arx), pred_arx, col = "blue", lwd = 2)




# ----------
# test data
pred_arx_t <- predict(modarx, zr1)

output_t <- c(zr1$output)


par(mfrow = c(1,1))

plot(output_t, type = "l")

lines(time(pred_arx_t), pred_arx_t, col = "blue", lwd = 2)



resid_arx <- pred_arx - output

resid_arx_t <- pred_arx_t - output_t




# ----------
sysid::residplot(modarx)




# ----------
# bode plot for predicted and original series

tmp <- idframe(output = ts(pred_arx), input = ts(ze1$input), Ts = 1)

tmp_t <- idframe(output = ts(pred_arx_t), input = ts(zr1$input), Ts = 1)

freq <- c(spa(ze1)$freq)

ori_gain <- 20 * log10(c(abs(spa(ze1)$response)))
train_gain_arx <- 20 * log10(c(abs(spa(tmp)$response)))
test_gain_arx <- 20 * log10(c(abs(spa(tmp_t)$response)))

ori_phase <- atan2(y = c(Im(spa(ze1)$response)), x = c(Re(spa(ze1)$response))) / (2 * pi) * 360
train_phase_arx <- atan2(y = c(Im(spa(tmp)$response)), x = c(Re(spa(tmp)$response))) / (2 * pi) * 360
test_phase_arx <- atan2(y = c(Im(spa(tmp_t)$response)), x = c(Re(spa(tmp_t)$response))) / (2 * pi) * 360


graphics.off()
par(mfrow = c(2,1))

plot(ori_gain ~ freq, type = "l", lty = 2, col = "black", lwd = 2, ylim = c(-40, 20), 
     main = "Magnitude : original (black), train (blue), test (red)")
lines(train_gain_arx ~ freq, type = "l", lty = 1, col = "blue", lwd = 2)
lines(test_gain_arx ~ freq, type = "l", lty = 1, col = "red", lwd = 2)
abline(h = seq(-40, 20, by = 5), v = seq(0, 3.0, by = 0.25), lty = 2, col = "gray")
abline(h = 0, v = seq(0, 3.0, by = 1), lty = 2, col = "black")

plot(ori_phase ~ freq, type = "l", lty = 2, col = "black", lwd = 2, ylim = c(-200, 200),
     main = "Phase : original (black), train (blue), test (red)")
lines(train_phase_arx ~ freq, type = "l", lty = 1, col = "blue", lwd = 2)
lines(test_phase_arx ~ freq, type = "l", lty = 1, col = "red", lwd = 2)
abline(h = seq(-200, 200, by = 25), v = seq(0, 3.0, by = 0.25), lty = 2, col = "gray")
abline(h = 0, v = seq(0, 3.0, by = 1), lty = 2, col = "black")




###########################################
# transfer funciton and its bode plot

modarx$sys

( num <- modarx$sys$B )

( den <- modarx$sys$A )



library(control)


# transfer function
( sys <- tf(num = num, den = den) )


pole(sys)

tf2zp(sys)




# impulse response for this system  --> unstable !!
impulseplot(sys)


lsimplot(sys, u = c(ze1$input), t = 1:601)



# bode plot
bodeplot(sys)




# ------------------------------------------------------------------------------
# select best Output-Error (OE) model
# ------------------------------------------------------------------------------


( parms <- expand.grid(na = 1:10, nb = 1:10, nk = 0:5) )


parms$aicc <- "NA"
parms$fpe <- "NA"
parms$naic <- "NA"
parms$bic <- "NA"
parms$mse <- "NA"


for(i in 1:nrow(parms)){
  
  na <- parms[i,"na"]
  nb <- parms[i,"nb"]
  nk <- parms[i,"nk"]
  
  e <- try(
    modoe0 <- oe(ze1, order = c(na, nb, nk)),
    silent = TRUE
  )
  if( class(e) == "try-error") {
  }else{
    parms$aicc[i] <- fitch(modoe0)$AICc
    parms$fpe[i] <- fitch(modoe0)$FPE
    parms$naic[i] <- fitch(modoe0)$nAIC
    parms$bic[i] <- fitch(modoe0)$BIC
    parms$mse[i] <- fitch(modoe0)$MSE
  }
}



parms


parms[which.min(parms$aicc),]
parms[which.min(parms$fpe),]
parms[which.min(parms$naic),]
parms[which.min(parms$bic),]
parms[which.min(parms$mse),]





# ----------
# now we try na = 9, nb = 3, nk = 0

modoe <- oe(ze1, order = c(9,3,0))


modoe$sys




# ----------
# test data
pred_oe <- predict(modoe, ze1)

pred_oe_t <- predict(modoe, zr1)


resid_oe <- pred_oe - output

resid_oe_t <- pred_oe_t - output_t



# ----------
sysid::residplot(modoe)



# ----------
# bode plot for predicted and original series

tmp <- idframe(output = ts(pred_oe), input = ts(ze1$input), Ts = 1)

tmp_t <- idframe(output = ts(pred_oe_t), input = ts(zr1$input), Ts = 1)

train_gain_oe <- 20 * log10(c(abs(spa(tmp)$response)))
test_gain_oe <- 20 * log10(c(abs(spa(tmp_t)$response)))

train_phase_oe <- atan2(y = c(Im(spa(tmp)$response)), x = c(Re(spa(tmp)$response))) / (2 * pi) * 360
test_phase_oe <- atan2(y = c(Im(spa(tmp_t)$response)), x = c(Re(spa(tmp_t)$response))) / (2 * pi) * 360


graphics.off()
par(mfrow = c(2,1))

plot(ori_gain ~ freq, type = "l", lty = 2, col = "black", lwd = 2, ylim = c(-40, 20), 
     main = "Magnitude : original (black), train (blue), test (red)")
lines(train_gain_oe ~ freq, type = "l", lty = 1, col = "blue", lwd = 2)
lines(test_gain_oe ~ freq, type = "l", lty = 1, col = "red", lwd = 2)
abline(h = seq(-40, 20, by = 5), v = seq(0, 3.0, by = 0.25), lty = 2, col = "gray")
abline(h = 0, v = seq(0, 3.0, by = 1), lty = 2, col = "black")

plot(ori_phase ~ freq, type = "l", lty = 2, col = "black", lwd = 2, ylim = c(-200, 200),
     main = "Phase : original (black), train (blue), test (red)")
lines(train_phase_oe ~ freq, type = "l", lty = 1, col = "blue", lwd = 2)
lines(test_phase_oe ~ freq, type = "l", lty = 1, col = "red", lwd = 2)
abline(h = seq(-200, 200, by = 25), v = seq(0, 3.0, by = 0.25), lty = 2, col = "gray")
abline(h = 0, v = seq(0, 3.0, by = 1), lty = 2, col = "black")




# ------------------------------------------------------------------------------
# Estimate ARMAX model model
# ------------------------------------------------------------------------------


( parms <- expand.grid(na = 1:5, nb = 1:5, nc = 1:3, nk = 0:5) )


parms$aicc <- "NA"
parms$fpe <- "NA"
parms$naic <- "NA"
parms$bic <- "NA"
parms$mse <- "NA"


for(i in 1:nrow(parms)){
  
  na <- parms[i,"na"]
  nb <- parms[i,"nb"]
  nc <- parms[i,"nc"]
  nk <- parms[i,"nk"]
  
  e <- try(
    modarmax0 <- armax(ze1, order = c(na, nb, nc, nk)),
    silent = TRUE
  )
  if( class(e) == "try-error") {
  }else{
    parms$aicc[i] <- fitch(modarmax0)$AICc
    parms$fpe[i] <- fitch(modarmax0)$FPE
    parms$naic[i] <- fitch(modarmax0)$nAIC
    parms$bic[i] <- fitch(modarmax0)$BIC
    parms$mse[i] <- fitch(modarmax0)$MSE
  }
}



parms


parms[which.min(parms$aicc),]
parms[which.min(parms$fpe),]
parms[which.min(parms$naic),]
parms[which.min(parms$bic),]
parms[which.min(parms$mse),]




# ----------
modarmax <- armax(ze1, order = c(4,5,1,0))


modarmax$sys




# ----------
# test data
pred_armax <- predict(modarmax, ze1)

pred_armax_t <- predict(modarmax, zr1)


resid_armax <- pred_armax - output

resid_armax_t <- pred_armax_t - output_t



# ----------
sysid::residplot(modarmax)




# ----------
# bode plot for predicted and original series

tmp <- idframe(output = ts(pred_armax), input = ts(ze1$input), Ts = 1)

tmp_t <- idframe(output = ts(pred_armax_t), input = ts(zr1$input), Ts = 1)

train_gain_armax <- 20 * log10(c(abs(spa(tmp)$response)))
test_gain_armax <- 20 * log10(c(abs(spa(tmp_t)$response)))

train_phase_armax <- atan2(y = c(Im(spa(tmp)$response)), x = c(Re(spa(tmp)$response))) / (2 * pi) * 360
test_phase_armax <- atan2(y = c(Im(spa(tmp_t)$response)), x = c(Re(spa(tmp_t)$response))) / (2 * pi) * 360


graphics.off()
par(mfrow = c(2,1))

plot(ori_gain ~ freq, type = "l", lty = 2, col = "black", lwd = 2, ylim = c(-40, 20), 
     main = "Magnitude : original (black), train (blue), test (red)")
lines(train_gain_armax ~ freq, type = "l", lty = 1, col = "blue", lwd = 2)
lines(test_gain_armax ~ freq, type = "l", lty = 1, col = "red", lwd = 2)
abline(h = seq(-40, 20, by = 5), v = seq(0, 3.0, by = 0.25), lty = 2, col = "gray")
abline(h = 0, v = seq(0, 3.0, by = 1), lty = 2, col = "black")

plot(ori_phase ~ freq, type = "l", lty = 2, col = "black", lwd = 2, ylim = c(-200, 200),
     main = "Phase : original (black), train (blue), test (red)")
lines(train_phase_armax ~ freq, type = "l", lty = 1, col = "blue", lwd = 2)
lines(test_phase_armax ~ freq, type = "l", lty = 1, col = "red", lwd = 2)
abline(h = seq(-200, 200, by = 25), v = seq(0, 3.0, by = 0.25), lty = 2, col = "gray")
abline(h = 0, v = seq(0, 3.0, by = 1), lty = 2, col = "black")





# ------------------------------------------------------------------------------
# Estimate BJ model
# ------------------------------------------------------------------------------

# add nc, nd, nf
# modbj <- bj(ze, order = c(2,2,2,1,1))


# modbj$sys




# ----------
# test data
# pred_bj <- predict(modbj, ze)

# pred_bj_t <- predict(modbj, zr)


# resid_bj <- pred_bj - output

# resid_bj_t <- pred_bj_t - output_t




# ------------------------------------------------------------------------------
# Model selection
# ------------------------------------------------------------------------------


round(unlist(fitch(modarx)), 3)

round(unlist(fitch(modoe)), 3)

round(unlist(fitch(modarmax)), 3)



# -->
# ARX model should not be compared ... (calculation is influenced by some regularization)

# ARMAX model is best




# ------------------------------------------------------------------------------
# Compare models prediction and residulas
# ------------------------------------------------------------------------------

# prediction for train data and test data
graphics.off()
par(mfcol = c(2,3))

plot(output, type = "l", main = "ARX train")
lines(time(pred_arx), pred_arx, col = "blue", lwd = 2)

plot(output_t, type = "l", main = "ARX test")
lines(time(pred_arx_t), pred_arx_t, col = "blue", lwd = 2)

plot(output, type = "l", main = "OE train")
lines(time(pred_oe), pred_oe, col = "red", lwd = 2)

plot(output_t, type = "l", main = "OE test")
lines(time(pred_oe_t), pred_oe_t, col = "red", lwd = 2)

plot(output, type = "l", main = "ARMAX train")
lines(time(pred_armax), pred_armax, col = "black", lwd = 2)

plot(output_t, type = "l", main = "ARMAX test")
lines(time(pred_armax_t), pred_armax_t, col = "black", lwd = 2)



# -->
# Note that OE model is worst, disturbunces shoudl be correlated




# ------------
# residuals for train data and test data
graphics.off()

par(mfcol = c(2,3))

plot(resid_arx, type = "l", main = "ARX train", ylim = c(-5,5))
abline(h = 0, col = "gray")

plot(resid_arx_t, type = "l", main = "ARX test", ylim = c(-5,5))
abline(h = 0, col = "gray")

plot(resid_oe, type = "l", main = "OE train", ylim = c(-5, 5))
abline(h = 0, col = "gray")

plot(resid_oe_t, type = "l", main = "OE test", ylim = c(-5, 5))
abline(h = 0, col = "gray")

plot(resid_armax, type = "l", main = "ARMAX train", ylim = c(-10, 10))
abline(h = 0, col = "gray")

plot(resid_armax_t, type = "l", main = "ARMAX test", ylim = c(-10, 10))
abline(h = 0, col = "gray")



# ----------
# mean absolute errors

mean(abs(resid_arx))

mean(abs(resid_arx_t))


mean(abs(resid_oe))

mean(abs(resid_oe_t))


mean(abs(resid_armax))

mean(abs(resid_armax_t))




# ----------
compare(zr1, modarx, modoe, modarmax, nahead = 1)




# ----------
# bode plot for train data, original and each models

graphics.off()
par(mfrow = c(2,1))

ori_gain <- 20 * log10(c(abs(spa(zr1)$response)))
ori_phase <- atan2(y = c(Im(spa(zr1)$response)), x = c(Re(spa(zr1)$response))) / (2 * pi) * 360


plot(ori_gain ~ freq, type = "l", lty = 2, col = "black", lwd = 2, ylim = c(-40, 20), 
     main = "Magnitude : Test original (black), ARX (orange), OE (blue), ARMAX (red)")
lines(test_gain_arx ~ freq, type = "l", lty = 1, col = "orange", lwd = 2)
lines(test_gain_oe ~ freq, type = "l", lty = 1, col = "blue", lwd = 2)
lines(test_gain_armax ~ freq, type = "l", lty = 1, col = "red", lwd = 2)
abline(h = seq(-40, 20, by = 5), v = seq(0, 3.0, by = 0.25), lty = 2, col = "gray")
abline(h = 0, v = seq(0, 3.0, by = 1), lty = 2, col = "black")

plot(ori_phase ~ freq, type = "l", lty = 2, col = "black", lwd = 2, ylim = c(-200, 200),
     main = "Phase : Test original (black), ARX (orange), OE (blue), ARMAX (red)")
lines(test_phase_arx ~ freq, type = "l", lty = 1, col = "orange", lwd = 2)
lines(test_phase_oe ~ freq, type = "l", lty = 1, col = "blue", lwd = 2)
lines(test_phase_armax ~ freq, type = "l", lty = 1, col = "red", lwd = 2)
abline(h = seq(-200, 200, by = 25), v = seq(0, 3.0, by = 0.25), lty = 2, col = "gray")
abline(h = 0, v = seq(0, 3.0, by = 1), lty = 2, col = "black")

