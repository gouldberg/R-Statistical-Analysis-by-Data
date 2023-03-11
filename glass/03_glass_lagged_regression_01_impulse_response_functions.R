setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\glass")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  glass
#   - Output:  Thickness
#   - Input:   Speed
# ------------------------------------------------------------------------------


glass_y <- read.table("glass_y.txt", sep = "", header = F, colClasses = "numeric")

glass_u <- read.table("glass_u.txt", sep = "", header = F, colClasses = "numeric")


glass <- cbind(glass_u, glass_y)


colnames(glass) <- c("input", "output")



head(glass)




# ----------
# demean input

glass$input2 <- glass$input - mean(glass$input)




# ------------------------------------------------------------------------------
# Regression with lagged variables:  impulse response function
# ------------------------------------------------------------------------------


# L:  degree of smoothing
# M:  must be even, number of terms used in the lagged regression, abs(t) >= M/2
# threshold:  the cut-off used to set small (in absolute value) regression coefficients equal to zero


input <- glass$input2

output <- glass$output


mod_l <- astsa::LagReg(input = input, output = output, L = 15, M = 40, threshold = 0.005)





# ------------------------------------------------------------------------------
# Regression with lagged variables:  impulse response function in inverse relation to find more simpler model
# ------------------------------------------------------------------------------

mod_i <- astsa::LagReg(input = output, output = input, L = 15, M = 40, inverse = TRUE,  threshold = 0.005)


