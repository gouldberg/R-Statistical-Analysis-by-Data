setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\dry2")


# note that DO NOT USE dplyr
packages <- c("sysid")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  dry2
# ------------------------------------------------------------------------------


u <- read.csv("dry2_u.txt", sep = "", header = F, colClasses = "numeric")

y <- read.csv("dry2_y.txt", sep = "", header = F, colClasses = "numeric")


dry2 <- cbind(u, y)


colnames(dry2) <- c("input", "output")


head(dry2)



# ----------
str(dry2)




# ------------------------------------------------------------------------------
# data preparation
# ------------------------------------------------------------------------------


# convert to idframe
dry <- idframe(output = ts(dry2$output), input = ts(dry2$input), Ts = 1)


str(dry)


# split data
ze <- dataSlice(dry, start = 100, end = 500)

zr <- dataSlice(dry, start = 501, end = 850)



# ----------
# detrending
# default is substracting mean value

# R by default doesn’t allow return of multiple objects.
# The %=% operator and g function in this package facillitate this behaviour

g(ze) %=% detrend(ze)

g(zr) %=% detrend(zr)


ze$output



# ----------
plot(dry)



par(mfrow = c(2,1))

plot(ze)



# -->
# 20 * log10(mean deviation ratio) = 13.65

20 * log10(mean(abs(ze$output)) / mean(abs(ze$input)))




# ------------------------------------------------------------------------------
# Estimate Frequency Response
# ------------------------------------------------------------------------------


# default lag size of the Hanning window (Default: min (length(x)/10,30))
# frequency points at which the response is evaluated (Default: seq(1,128)/128*pi/Ts --> up to 3.14)
sp <- spa(ze)


c(20 * log10(abs(sp$response)))



# bode plot
#  - Y: amplitude in decibel, phase in degree
#  - X: rad / unit time, 1 means 1 rad/s
plot(sp)





# ----------
# etfe:  Emperical Transfer Function Estimate
# from the data by taking the ratio of the fourier transforms of the output and the input variables
plot(etfe(ze))




# ------------------------------------------------------------------------------
# Impulse Response Function
# ------------------------------------------------------------------------------


fit <- impulseest(ze, M = 40)

sysid::impulseplot(fit)



# -->
# estimated delay = 3



# ----------
# step response function

st <- sysid::step(fit)

st$data




# ------------------------------------------------------------------------------
# Estimate ARX model parameter na, nb, nk
# ------------------------------------------------------------------------------


( parms <- expand.grid(na = 2:10, nb = 2:10, nk = 3) )



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
    modarx0 <- arx(ze, order = c(na, nb, nk)),
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
# now we try na = 2, nb = 2, nk = 3

modarx <- arx(ze, order = c(2,2,3), lambda = 0.1)

modarx$sys


unlist(fitch(modarx))




# ----------
# fitted values
pred_arx <- modarx$fitted.values[,1]

output <- c(ze$output)


par(mfrow = c(1,1))

plot(output, type = "l")

lines(time(pred_arx), pred_arx, col = "blue", lwd = 2)




# ----------
# test data
pred_arx_t <- predict(modarx, zr)

output_t <- c(zr$output)


par(mfrow = c(1,1))

plot(output_t, type = "l")

lines(time(pred_arx_t), pred_arx_t, col = "blue", lwd = 2)



resid_arx <- pred_arx - output

resid_arx_t <- pred_arx_t - output_t




# ----------
sysid::residplot(modarx)




# ----------
# bode plot for predicted and original series

tmp <- idframe(output = ts(pred_arx), input = ts(ze$input), Ts = 1)

tmp_t <- idframe(output = ts(pred_arx_t), input = ts(zr$input), Ts = 1)

freq <- c(spa(ze)$freq)

ori_gain <- 20 * log10(c(abs(spa(ze)$response)))
train_gain_arx <- 20 * log10(c(abs(spa(tmp)$response)))
test_gain_arx <- 20 * log10(c(abs(spa(tmp_t)$response)))

ori_phase <- atan2(y = c(Im(spa(ze)$response)), x = c(Re(spa(ze)$response))) / (2 * pi) * 360
train_phase_arx <- atan2(y = c(Im(spa(tmp)$response)), x = c(Re(spa(tmp)$response))) / (2 * pi) * 360
test_phase_arx <- atan2(y = c(Im(spa(tmp_t)$response)), x = c(Re(spa(tmp_t)$response))) / (2 * pi) * 360


graphics.off()
par(mfrow = c(2,1))

plot(ori_gain ~ freq, type = "l", lty = 2, col = "black", lwd = 2, ylim = c(-30, 20), 
     main = "Magnitude : original (black), train (blue), test (red)")
lines(train_gain_arx ~ freq, type = "l", lty = 1, col = "blue", lwd = 2)
lines(test_gain_arx ~ freq, type = "l", lty = 1, col = "red", lwd = 2)
abline(h = seq(-30, 20, by = 5), v = seq(0, 3.0, by = 0.25), lty = 2, col = "gray")
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



# -->
# all poles > 0 --> unstable



# impulse response for this system  --> unstable !!
impulseplot(sys)


lsimplot(sys, u = c(ze$input), t = 1:401)



# bode plot
bodeplot(sys)




# ------------------------------------------------------------------------------
# select best Output-Error (OE) model
# ------------------------------------------------------------------------------


( parms <- expand.grid(na = 1:10, nb = 1:10, nk = 3) )


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
    modoe0 <- oe(ze, order = c(na, nb, nk)),
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
# now we try na = 2, nb = 2, nk = 3

modoe <- oe(ze, order = c(2,2,3))


modoe$sys




# ----------
# test data
pred_oe <- predict(modoe, ze)

pred_oe_t <- predict(modoe, zr)


resid_oe <- pred_oe - output

resid_oe_t <- pred_oe_t - output_t



# ----------
sysid::residplot(modoe)



# ----------
# bode plot for predicted and original series

tmp <- idframe(output = ts(pred_oe), input = ts(ze$input), Ts = 1)

tmp_t <- idframe(output = ts(pred_oe_t), input = ts(zr$input), Ts = 1)

train_gain_oe <- 20 * log10(c(abs(spa(tmp)$response)))
test_gain_oe <- 20 * log10(c(abs(spa(tmp_t)$response)))

train_phase_oe <- atan2(y = c(Im(spa(tmp)$response)), x = c(Re(spa(tmp)$response))) / (2 * pi) * 360
test_phase_oe <- atan2(y = c(Im(spa(tmp_t)$response)), x = c(Re(spa(tmp_t)$response))) / (2 * pi) * 360


graphics.off()
par(mfrow = c(2,1))

plot(ori_gain ~ freq, type = "l", lty = 2, col = "black", lwd = 2, ylim = c(-30, 20), 
     main = "Magnitude : original (black), train (blue), test (red)")
lines(train_gain_oe ~ freq, type = "l", lty = 1, col = "blue", lwd = 2)
lines(test_gain_oe ~ freq, type = "l", lty = 1, col = "red", lwd = 2)
abline(h = seq(-30, 20, by = 5), v = seq(0, 3.0, by = 0.25), lty = 2, col = "gray")
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


( parms <- expand.grid(na = 1:5, nb = 1:5, nc = 1:3, nk = 3) )


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
    modarmax0 <- armax(ze, order = c(na, nb, nc, nk)),
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
modarmax <- armax(ze, order = c(1,5,1,3))


modarmax$sys




# ----------
# test data
pred_armax <- predict(modarmax, ze)

pred_armax_t <- predict(modarmax, zr)


resid_armax <- pred_armax - output

resid_armax_t <- pred_armax_t - output_t



# ----------
sysid::residplot(modarmax)




# ----------
# bode plot for predicted and original series

tmp <- idframe(output = ts(pred_armax), input = ts(ze$input), Ts = 1)

tmp_t <- idframe(output = ts(pred_armax_t), input = ts(zr$input), Ts = 1)

train_gain_armax <- 20 * log10(c(abs(spa(tmp)$response)))
test_gain_armax <- 20 * log10(c(abs(spa(tmp_t)$response)))

train_phase_armax <- atan2(y = c(Im(spa(tmp)$response)), x = c(Re(spa(tmp)$response))) / (2 * pi) * 360
test_phase_armax <- atan2(y = c(Im(spa(tmp_t)$response)), x = c(Re(spa(tmp_t)$response))) / (2 * pi) * 360


graphics.off()
par(mfrow = c(2,1))

plot(ori_gain ~ freq, type = "l", lty = 2, col = "black", lwd = 2, ylim = c(-30, 20), 
     main = "Magnitude : original (black), train (blue), test (red)")
lines(train_gain_armax ~ freq, type = "l", lty = 1, col = "blue", lwd = 2)
lines(test_gain_armax ~ freq, type = "l", lty = 1, col = "red", lwd = 2)
abline(h = seq(-30, 20, by = 5), v = seq(0, 3.0, by = 0.25), lty = 2, col = "gray")
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
compare(zr, modarx, modoe, modarmax, nahead = 1)




# ----------
# bode plot for train data, original and each models

graphics.off()
par(mfrow = c(2,1))

ori_gain <- 20 * log10(c(abs(spa(zr)$response)))
ori_phase <- atan2(y = c(Im(spa(zr)$response)), x = c(Re(spa(zr)$response))) / (2 * pi) * 360


plot(ori_gain ~ freq, type = "l", lty = 2, col = "black", lwd = 2, ylim = c(-30, 20), 
     main = "Magnitude : Test original (black), ARX (orange), OE (blue), ARMAX (red)")
lines(test_gain_arx ~ freq, type = "l", lty = 1, col = "orange", lwd = 2)
lines(test_gain_oe ~ freq, type = "l", lty = 1, col = "blue", lwd = 2)
lines(test_gain_armax ~ freq, type = "l", lty = 1, col = "red", lwd = 2)
abline(h = seq(-30, 20, by = 5), v = seq(0, 3.0, by = 0.25), lty = 2, col = "gray")
abline(h = 0, v = seq(0, 3.0, by = 1), lty = 2, col = "black")

plot(ori_phase ~ freq, type = "l", lty = 2, col = "black", lwd = 2, ylim = c(-200, 200),
     main = "Phase : Test original (black), ARX (orange), OE (blue), ARMAX (red)")
lines(test_phase_arx ~ freq, type = "l", lty = 1, col = "orange", lwd = 2)
lines(test_phase_oe ~ freq, type = "l", lty = 1, col = "blue", lwd = 2)
lines(test_phase_armax ~ freq, type = "l", lty = 1, col = "red", lwd = 2)
abline(h = seq(-200, 200, by = 25), v = seq(0, 3.0, by = 0.25), lty = 2, col = "gray")
abline(h = 0, v = seq(0, 3.0, by = 1), lty = 2, col = "black")

