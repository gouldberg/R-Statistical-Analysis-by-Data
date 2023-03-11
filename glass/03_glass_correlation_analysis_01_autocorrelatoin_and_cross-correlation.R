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
# Correlation analysis:  autocorrelation and partial-correaltion
# ------------------------------------------------------------------------------

graphics.off()


astsa::acf2(glass$output, max.lag = 50, main = "Output")


astsa::acf2(glass$input2, max.lag = 50, main = "Input")





# ----------
astsa::sarima(p = 5, d = 0, q = 1, xdata = glass$output)


astsa::sarima(p = 7, d = 0, q = 1, xdata = glass$input2)





# ------------------------------------------------------------------------------
# Correlation analysis:  cross-correlation
# ------------------------------------------------------------------------------


input <- glass$input2

output <- glass$output


ccf(output, input, lag = 50)



# -->
# output has negative correlation peaks at lag 11


