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
# Gain and Phase of the response from Input to Output
# ------------------------------------------------------------------------------

library(timsac)



# maximum lag by default = 2 * sqrt(nrow(gasf)) = 34.4
output <- sglfre(as.matrix(gasf), lag = NULL, invar = 1, outvar = 2)




graphics.off()

par(mfrow = c(2,2))

plot(output$gain, type = "l", main = "Gain")

plot(output$phase, type = "l", main = "Phase")

plot(output$coh, type = "l", main = "Coherence")



# plot(output$freqi[1:10] ~ output$freqr[1:10], type = "l", main = "Phase")


