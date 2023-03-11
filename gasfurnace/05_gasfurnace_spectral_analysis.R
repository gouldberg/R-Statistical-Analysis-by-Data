
packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Gas Furnace
# ------------------------------------------------------------------------------


gasf <- read.table("GasFurnace.txt", sep = "", header = F, colClasses = "numeric")


colnames(gasf) <- c("input", "output")


head(gasf)




# ------------------------------------------------------------------------------
# Spectral analysis:  Raw periodogram
# ------------------------------------------------------------------------------


input <- gasf$input
output <- gasf$output


input <- diff(gasf$input)
output <- diff(gasf$output)



nextn(length(input))



# ----------
par(mfrow=c(2,1))


input.per <- astsa::mvspec(input, log = "no")

output.per <- astsa::mvspec(output, log = "no")




# ------------------------------------------------------------------------------
# Spectral analysis:  averaged periodogram
# ------------------------------------------------------------------------------

# 296 * 5% = 15
L <- 15

m <- (L - 1) / 2



# ----------
par(mfrow=c(2,1))


# To compute averaged periodograms, use the Daniell kernel, and specify m, where L = 2m + 1
input.ave <- astsa::mvspec(input, kernel("daniell", m), log = "no")

output.ave <- astsa::mvspec(output, kernel("daniell", m), log = "no")




# ------------------------------------------------------------------------------
# Spectral analysis:  smoothed periodogram by modified Daniell kernel with taper
# ------------------------------------------------------------------------------


( ker <- kernel("modified.daniell", c(3,3)) )

plot(ker)



graphics.off()

par(mfrow=c(2,1))

input.smo <- astsa::mvspec(input, kernel = ker, taper = 0.1, log = "no")

output.smo <- astsa::mvspec(output, kernel = ker, taper = 0.1, log = "no")




# ------------------------------------------------------------------------------
# Spectral analysis:  output from "mvspec"
# ------------------------------------------------------------------------------


# L <- 9
L <- 15

( m <- (L - 1) / 2 )


sr <- mvspec(cbind(output, input), kernel("daniell", m), plot = TRUE)



# ----------
# from spectrum matrix

fyy <- sr$fxx[1,1,]

fxx <- sr$fxx[2,2,]




# ------------------------------------------------------------------------------
# Spectral analysis:  output from "timsac"
# ------------------------------------------------------------------------------


library(timsac)


# maximum lag by default = 2 * sqrt(nrow(gasf)) = 34.4
sgl_out <- sglfre(as.matrix(gasf), invar = 1, outvar = 2, lag = 34)



# this is power spectrum
sgl_out$outspec

sgl_out$inspec




# ------------------------------------------------------------------------------
# Spectral analysis:  compare output
# ------------------------------------------------------------------------------


graphics.off()

par(mfrow = c(2,2))


sr <- mvspec(cbind(output, input), kernel("daniell", m), plot = TRUE)


plot(sr$spec[,1] ~ sr$freq , type = "l", lty = 1, lwd = 2, xlab = "frequency", ylab = "spectrum", main = "Spectrum: Output and Input from sr$spec")

lines(sr$spec[,2] ~ sr$freq, type = "l", lty = 2, lwd = 1)


plot(abs(fyy) ~ sr$freq, type = "l", lty = 1, lwd = 2, xlab = "frequency", ylab = "spectrum", main = "Spectrum: Output and Input from sr$fxx")

lines(abs(fxx) ~ sr$freq, type = "l", lty = 2, lwd = 1)


plot(sgl_out$outspec / 2, type = "l", lty = 1, lwd = 2, ylab = "spectrum", main = "Spectrum: Output and Input from sglfre")

lines(sgl_out$inspec / 2, type = "l", lty = 2, lwd = 1)
