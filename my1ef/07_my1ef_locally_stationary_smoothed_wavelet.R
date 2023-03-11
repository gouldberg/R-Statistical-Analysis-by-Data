setwd("C:\\Users\\kswad\\Desktop\\R\\my1ef")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  MYE1F
# ------------------------------------------------------------------------------

dat <- read.table("MYE1F.txt", sep = "", header = T, colClasses = "numeric")


head(dat)


dat <- dat$x



# ------------------------------------------------------------------------------
# Locally Stationary Corrected Smoothed Wavelet
# ------------------------------------------------------------------------------

library(wavethresh)



# compute corrected smoothed wavelet periodgram of differenced series
# Both LSW (locally stationary wavelet) discrete wavelets and smoothing wavelets are
# Daubechies' least-asymmetric wavelets with 10 vanishing moments.
# Each periodogram was level smoothed by log transform,
# followed by translation-invariant global universal thresholding with MAD variance
# estimation on all smoothed levels (4:10),
# followed by inverse exp transform

spec <- ewspec(dat[1:(2^11)],
               smooth.levels = 4:10,
               smooth.policy = "universal",
               smooth.transform = log,
               smooth.inverse = exp)$S

graphics.off()
par(mfrow = c(2,1))

plot(dat[1:(2^11)], type = "l")
abline(v = c(630, 1026), col = "blue")

plot(spec, main = "", sub = "", 
     scaling = "by.level", ylab = "Scale")
abline(v = c(630, 1026), col = "blue")


