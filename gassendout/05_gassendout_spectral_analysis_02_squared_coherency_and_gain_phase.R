grasetwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\gassendout")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Gas Sendout
# ------------------------------------------------------------------------------

gas <- read.table("GasSendout.txt", sep = "", header = F, colClasses = "numeric")



colnames(gas) <- c("MetI", "gasSendout", "V3")


head(gas)





# ------------------------------------------------------------------------------
# Spectral analysis
#   - Coherence:  correlation indexed by frequency
#   - Squared coherence:
#        An important example of the application of the cross-spectrum is to the problem of predicting an output series y(t) from
#        some input series x(t) through a linear filter relation such as the three-point moving average.
#        A measure of the strength of such a relation is the squared coherence.
# ------------------------------------------------------------------------------



L <- 15

( m <- (L - 1) / 2 )


sr <- mvspec(cbind(output, input), kernel("daniell", m), plot = FALSE)




# ----------
sr$df


# significance level alpha = 0.001
( f <- qf(.999, 2, sr$df -2) )

( C <- f / (L - 1 + f) )


par(mfrow = c(2,1))

plot(sr)

plot(sr, plot.type = "coh", ci.lty = 2)

abline(h = C)




# ------------------------------------------------------------------------------
# Spectral analysis:  output from "mvspec"
# ------------------------------------------------------------------------------


fyy <- sr$fxx[1,1,]

fxx <- sr$fxx[2,2,]



# ----------
# cross-spectrum
fyx <- sr$fxx[1,2,]


# frequency response
Ayx <- fyx / fxx


# cospectrum and quadspectrum
cyx <- Re(Ayx)
  
qyx <- - Im(Ayx)



# ----------
# Amplitude
Ayx_norm <- abs(Ayx)



# ----------
# phase
Phiyx <- atan(- qyx / cyx)



# ----------
# squared coherence
rho2yx <- Re(abs(fyx)^2 / (fxx * fyy))




# ------------------------------------------------------------------------------
# Spectral analysis:  output from "timsac"
# ------------------------------------------------------------------------------

library(timsac)



# maximum lag by default = 2 * sqrt(nrow(gasf)) = 34.4
sgl_out <- sglfre(as.matrix(cbind(input, output)), invar = 1, outvar = 2, lag = 34)




# ------------------------------------------------------------------------------
# Compare output:  Squared Coherency
# ------------------------------------------------------------------------------


graphics.off()

par(mfrow = c(2,2))


plot(sr, plot.type = "coh", ylim = c(0, 1.0))

plot(sr$coh ~ sr$freq, type = "l", lty = 1, lwd = 2, 
     xlab = "frequency", ylab = "Squared Coherence", main = "Squared Coherency from sr$coh",
     ylim = c(0, 1.0))

plot(rho2yx ~ sr$freq, type = "l", lty = 1, lwd = 2, 
     xlab = "frequency", ylab = "Squared Coherence", main = "Squared Coherency from sr$fxx",
     ylim = c(0, 1.0))


# sglfre:  esimate based on aligned time series (lag 5 for input maybe)
plot(sgl_out$coh, type = "l", lty = 1, lwd = 2, 
     xlab = "frequency", ylab = "Squared Coherence", main = "Squared Coherency from sglfre",
     ylim = c(0, 1.0))




# ------------------------------------------------------------------------------
# Compare output:  Squared Frequency Response and Frequency Response
# ------------------------------------------------------------------------------

graphics.off()

par(mfrow = c(2,2))


plot(abs(Ayx)^2 ~ sr$freq, type = "l", lty = 1, lwd = 2, 
     xlab = "frequency", ylab = "Squared Fequency Response", main = "Squared Frequency Response")

plot(cyx ~ sr$freq, type = "h",
     xlab = "frequency", ylab = "Cospectrum", main = "Frequency Response: Cospectrum")
abline(h = 0, lty = 2, col = "gray")

plot(qyx ~ sr$freq, type = "h", 
     xlab = "frequency", ylab = "Quadspectrum", main = "Frequency Response: Quadpectrum")
abline(h = 0, lty = 2, col = "gray")

plot(Phiyx ~ sr$freq, type = "l", 
     xlab = "frequency", ylab = "Phase", main = "Frequency Response: Phase")
abline(h = 0, lty = 2, col = "gray")




# ------------------------------------------------------------------------------
# output from timsac:  Frequency Response  (but note that this is in time domain representation ...)
# ------------------------------------------------------------------------------

graphics.off()

par(mfrow = c(2,2))


plot(sgl_out$freqr, type = "l", main = "Frequency Response: Real Part")
abline(h = 0, lty = 2, col = "gray")


plot(sgl_out$freqi, type = "l", main = "Frequency Response: Imagenary Part")
abline(h = 0, lty = 2, col = "gray")

plot(sgl_out$cspec, type = "h", 
     ylab = "Cospectrum", main = "Frequency Response: Cospectrum")
abline(h = 0, lty = 2, col = "gray")

plot(sgl_out$qspec, type = "h", 
     ylab = "Quadspectrum", main = "Frequency Response: Quadpectrum")
abline(h = 0, lty = 2, col = "gray")



# ----------
graphics.off()

par(mfrow = c(1,1))


plot(sgl_out$freqi ~ sgl_out$freqr, type = "l", main = "Frequency Response in I and R", xlab = "Real Part", ylab = "Imagenary Part")
abline(h = 0, v = 0, lty = 2, col = "gray")





# ------------------------------------------------------------------------------
# output from timsac:  Gain and Phase
# ------------------------------------------------------------------------------

graphics.off()

par(mfrow = c(2,1))


# Gain might be based on the aligned time series (input lag 5)
plot(sgl_out$gain, type = "l", main = "Frequency Response: Gain", ylab = "Gain")

plot(sgl_out$phase / (2 * pi), type = "l", main = "Frequency Response: Phase", ylab = "Phase")




# ----------
# Manual calculation for Gain

cyx2 <- sgl_out$cspec

qyx2 <- sgl_out$qspec


fxx2 <- sgl_out$inspec / 2

fyy2 <- sgl_out$outspec / 2


Ayx2 <- ( sqrt(cyx2^2 + qyx2^2) / fxx2 )


# this is Gain
( gain <- Ayx2 / 2 )

sgl_out$gain




# ----------
# Manual calculation for Phase  --> but different

plot(atan(- qyx2 / cyx2), type = "l", lty = 1)


