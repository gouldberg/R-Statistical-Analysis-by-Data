setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\gasfurnace")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Gas Furnace
#   - Series of input and output measurements on a fas furnace that have been previously modeled by spectral analysis
#     in Jenkins and Watts (1968), and by lagged correlation methods in Box and Jenkins (1970).
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

L <- 15

m <- (L - 1) / 2



# ----------
par(mfrow=c(2,1))


# To compute averaged periodograms, use the Daniell kernel, and specify m, where L = 2m + 1
input.ave <- astsa::mvspec(input, kernel("daniell", m), log = "no")

output.ave <- astsa::mvspec(output, kernel("daniell", m), log = "no")




# ------------------------------------------------------------------------------
# Spectral analysis:  smoothed periodogram by modified Daniell kernel
# ------------------------------------------------------------------------------


( ker <- kernel("modified.daniell", c(5,5)) )

plot(ker)



graphics.off()
par(mfrow=c(2,1))

input.smo <- astsa::mvspec(input, kernel = ker, taper = 0.1, log = "no")
output.smo <- astsa::mvspec(output, kernel = ker, taper = 0.1, log = "no")



