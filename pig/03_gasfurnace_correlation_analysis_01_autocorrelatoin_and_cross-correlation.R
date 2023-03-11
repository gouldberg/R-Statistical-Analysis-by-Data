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
# Correlation analysis:  autocorrelation and partial-correaltion
# ------------------------------------------------------------------------------

graphics.off()

astsa::acf2(gasf$input, max.lag = 20, main = "Input")


astsa::acf2(diff(gasf$input), max.lag = 20, main = "Input")


astsa::sarima(p = 2, d = 1, q = 2, xdata = gasf$input)





# ----------

astsa::acf2(gasf$output, max.lag = 20, main = "Output")


astsa::acf2(diff(gasf$output), max.lag = 20, main = "output")


astsa::sarima(p = 3, d = 1, q = 3, xdata = gasf$output)




# ------------------------------------------------------------------------------
# Correlation analysis:  cross-correlation
# ------------------------------------------------------------------------------


input <- diff(gasf$input)
output <- diff(gasf$output)


input <- gasf$input
output <- gasf$output


ccf(output, input, lag = 20)



# -->
# output has negative correlation peaks at lag 5




# ----------
astsa::lag2.plot(output, input, max.lag = 20)



