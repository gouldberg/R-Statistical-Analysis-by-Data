
packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Gas Furnace
# ------------------------------------------------------------------------------


gasf <- read.table("GasFurnace.txt", sep = "", header = F, colClasses = "numeric")


colnames(gasf) <- c("input", "output")


head(gasf)





# ------------------------------------------------------------------------------
# Correlation analysis:  autocorrelation and partial-correaltion
# ------------------------------------------------------------------------------

graphics.off()

astsa::acf2(gasf$input, max.lag = 50, main = "Input")

astsa::sarima(p = 3, d = 0, q = 6, xdata = gasf$input)



# ----------
astsa::acf2(diff(gasf$input), max.lag = 50, main = "Input")

astsa::sarima(p = 3, d = 1, q = 1, xdata = gasf$input)




# ----------
astsa::acf2(gasf$output, max.lag = 50, main = "Output")

astsa::sarima(p = 1, d = 0, q = 5, xdata = gasf$output)



# ----------
astsa::acf2(diff(gasf$output), max.lag = 50, main = "output")

astsa::sarima(p = 3, d = 1, q = 2, xdata = gasf$output)




# ------------------------------------------------------------------------------
# Correlation analysis:  cross-correlation
# ------------------------------------------------------------------------------


input <- diff(gasf$input)
output <- diff(gasf$output)


input <- gasf$input
output <- gasf$output


ccf(output, input, lag = 50)


acf(cbind(output, input))



# -->
# output has negative correlation peaks at lag 5




# ----------
tmp <- data.frame(output = diff(gasf$output),  input = diff(gasf$input))

Acf(tmp, lag = 20)




# ----------
astsa::lag2.plot(output, input, max.lag = 20)



