setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\dry2")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
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



# ----------
# detrending
output2 <- resid(lm(output ~ time(output), data = dry2))


# demean
dry2$input2 <- dry2$input - mean(dry2$input)




# ----------
# VAR model

library(vars)


input <- dry2$input2

output <- dry2$output


x <- cbind(input, output)

fit5 <- VAR(x, p = 5, type = "both")

fit6 <- VAR(x, p = 6, type = "both")



# ------------------------------------------------------------------------------
# Simulation of Feedback Contoller:  Proportional control
# ------------------------------------------------------------------------------


# Select Reference value
R <- dry2$output


n <- length(R)



# ----------
# proportional control

Kp <- 0.8

C <- rep(Kp, n)



# ----------
# value for transfer function:  P  = b / (s + a)
# --> impulse response function y(t) = K / T * exp(-1 / T * t)

a <- 0.007

b <- 1

T <- 1 / a

K <- b / a

t <- 1:n
irf <- K / T * exp(-1 / T * t)



# ----------
# disturbance
D <- rnorm(n = n, mean = 0, sd = 0.5)




# ----------
# simulate Y (object to be controlled)

Y <- E <- U <- rep(0, n)

Y[1] <- mean(R)



for(t in 2:(n-1)){

  # E:  error
  E[t] <- R[t] - Y[t-1]
  
  
  # U:  operational input to Y  (proportional control)
  U[t] <- C[t] * E[t]
  
  
  # D:  disturbance
  # P[s] = b / (s + a) --> K/T * exp(-1/T*t)
  for(t2 in t:(n-1)){
    Y[t2] <- Y[t2] + K / T * exp(- 1 / T * t2) * (U[t] + D[t])
  }
}




# ----------
# plot output

graphics.off()

par(mfrow = c(2,2))


plot(R, type = "l", lwd = 2, col = "black", main = "R(black): Reference   Y(blue): object")
lines(Y, lwd = 2, col = "blue")


plot(U + D, type = "l", lwd = 2, col = "red", main = "Red  =  U(black): Input to Controller + D(gray): disturbances")
lines(U , lwd = 1, col = "black")
lines(D, lwd = 1, col = "gray")


plot(irf, type = "l", lwd = 2, col = "black", main = paste0("P: IRF     K = ", K, "  T = ", T))

plot(E, type = "l", lwd = 1, col = "black", main = "E: error")
abline(h = 0, lty = 2, col = "gray")




############################################################################################################################
############################################################################################################################

# ------------------------------------------------------------------------------
# Spectral analysis
#   - Coherence:  correlation indexed by frequency
#   - Squared coherence:
# ------------------------------------------------------------------------------

input <- R

output <- Y


# L <- 9
L <- 19

( m <- (L - 1) / 2 )


sr <- astsa::mvspec(cbind(output, input), kernel("daniell", m), plot = FALSE)




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
sgl_out <- sglfre(as.matrix(cbind(input, output)), invar = 1, outvar = 2, lag = 100)




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




