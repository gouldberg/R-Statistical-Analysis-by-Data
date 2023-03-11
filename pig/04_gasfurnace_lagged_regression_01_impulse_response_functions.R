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
# Regression with lagged variables:  impulse response function
# ------------------------------------------------------------------------------


# L:  degree of smoothing
# M:  must be even, number of terms used in the lagged regression, abs(t) >= M/2
# threshold:  the cut-off used to set small (in absolute value) regression coefficients equal to zero


input <- gasf$input

output <- gasf$output


mod_stor <- astsa::LagReg(input = input, output = output, L = 15, M = 40, threshold = 0.05)





# ------------------------------------------------------------------------------
# Regression with lagged variables:  impulse response function in inverse relation to find more simpler model
# ------------------------------------------------------------------------------

mod_rtos <- astsa::LagReg(input = output, output = input, L = 15, M = 40, inverse = TRUE,  threshold = 0.01)



# -->
# inverse model looks similar ..

