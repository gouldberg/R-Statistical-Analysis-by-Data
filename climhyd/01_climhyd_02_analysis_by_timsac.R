setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\climhyd")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  climhyd
#   - The data set contains 454 months of measured values for six climatic variables at Lake Shasta in California
#        - Temp:  air temperature
#        - DewPt:  DewPt
#        - CldCvr:  cloud cover
#        - WndSpd:  wind speed
#        - Precip:  precipitation
#        - Infrlow:  inflow
# ------------------------------------------------------------------------------


climhyd <- read.csv(file = "climhyd.txt", header = T, sep = "\t", stringsAsFactors = FALSE)


str(climhyd)


head(climhyd)




# ----------
inf <- log(climhyd$Inflow)

prec <- sqrt(climhyd$Precip)

Temp <- ts(climhyd$Temp, start = 1, frequency = 12)

DewPt <- ts(climhyd$DewPt, start = 1, frequency = 12)

WndSpd <- ts(climhyd$WndSpd, start = 1, frequency = 12)

cldcvr <- ts(climhyd$CldCvr, start = 1, frequency = 12)

prec <- ts(prec, start = 1, frequency = 12)

inf <- ts(inf, start = 1, frequency = 12)




# ------------------------------------------------------------------------------
# Spectral analysis:  gain and phase
# ------------------------------------------------------------------------------

library(timsac)


dat <- cbind(Temp, DewPt, WndSpd, cldcvr, prec, inf)


# maximum lag by default = 2 * sqrt(nrow(dat)) = 42.6
sgl_out <- sglfre(as.matrix(dat), invar = 5, outvar = 6, lag = 43)

sgl_out2 <- sglfre(as.matrix(dat), invar = 1, outvar = 6, lag = 43)



# ---------
graphics.off()

par(mfrow = c(2,1))


# Gain might be based on the aligned time series
plot(sgl_out$gain, type = "l", main = "Frequency Response: Gain  Inflow vs. Precip", ylab = "Gain")

plot(sgl_out2$gain, type = "l", main = "Frequency Response: Gain  Inflow vs. Temperature", ylab = "Gain")

# plot(sgl_out$phase / (2 * pi), type = "l", main = "Frequency Response: Phase", ylab = "Phase")




# ------------------------------------------------------------------------------
# Spectral analysis:  Relative Power Contribution
# ------------------------------------------------------------------------------

# h  frequencies
muln <- mulnos(as.matrix(dat), max.order = 30, h = 240)


str(muln$integr)




# ----------
graphics.off()

par(mfrow = c(2,3))

matplot(t(muln$integr[1,,]), type = "l")

matplot(t(muln$integr[2,,]), type = "l")

matplot(t(muln$integr[3,,]), type = "l")

matplot(t(muln$integr[4,,]), type = "l")

matplot(t(muln$integr[5,,]), type = "l")

matplot(t(muln$integr[6,,]), type = "l")




# ------------------------------------------------------------------------------
# Multivariate AR model
# ------------------------------------------------------------------------------

par(mar = c(2,2,2,2))


mulm <- mulmar(as.matrix(dat), plot = TRUE)


mulm$order.maice


mulm$aicmin


mulm$aicsum


mulm$arcoef





# ------------------------------------------------------------------------------
# locally stationary AR model fitting
# ------------------------------------------------------------------------------

graphics.off()

par(mfrow = c(1,1))


# span  length of the basic local span
mloc <- mlocar(inf, span = 50, plot = TRUE)



# ----------
# number of local spans and init and end point of current model
mloc$ns

mloc$init

mloc$end


# block length
mloc$nnew



# ----------
mloc$aic.mov




# ----------
graphics.off()

par(mfrow = c(1,1))

plot(c(inf), type = "l")

abline(v = c(43, 93, 143, 193, 243, 293), lty = 1, col = "blue", lwd = 2)



