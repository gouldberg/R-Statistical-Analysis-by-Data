setwd("//media//kswada//MyFiles//R//cmort")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  cmort (Pollution, Temperature and Mortality)
# ------------------------------------------------------------------------------

data(cmort, package = "astsa")
data(tempr, package = "astsa")
data(part, package = "astsa")


str(cmort)
str(tempr)
str(part)


cmort
tempr
part



# ------------------------------------------------------------------------------
# Correlation analysis:  Cross-correlation
# ------------------------------------------------------------------------------


graphics.off()


acf(cbind(cmort, tempr, part), cex.main = 2)





# -->
# Note that "cmrt & tempr lag" means:  cmort needs lag 4 (delay) to match with tempr

# mortality vs. pollution:  positive cross-correlation at lag 4 weeks
# mortality vs. temperature:  negative cross-correlation at lag 1 weeks




# ----------
# This is weekly data.

par(mfrow = c(3,1), mar = c(2,2,2,2))

plot(cmort, main = "Cardiovascular Mortality", xlab = "", ylab = "")
abline(v = seq(1970, 1980, by = 1), col = "gray", lty = 2)

plot(tempr, main = "Temerature", xlab = "", ylab = "")
abline(v = seq(1970, 1980, by = 1), col = "gray", lty = 2)

plot(part, main = "Particulates", xlab = "", ylab = "")
abline(v = seq(1970, 1980, by = 1), col = "gray", lty = 2)


