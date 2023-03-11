
packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Gas Furnace
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


mod_l <- astsa::LagReg(input = input, output = output, L = 3, M = 40, threshold = 0.05)



# -->
# lag 6




# ------------------------------------------------------------------------------
# Regression with lagged variables:  impulse response function in inverse relation to find more simpler model
# ------------------------------------------------------------------------------

mod_i <- astsa::LagReg(input = output, output = input, L = 3, M = 40, inverse = TRUE,  threshold = 0.01)



# -->
# inverse model looks similar ..

