setwd("//media//kswada//MyFiles//R//oil_gas")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Oil and Gas
# ------------------------------------------------------------------------------

data(oil, package = "astsa")
data(gas, package = "astsa")

str(oil)
str(gas)

oil
gas



# ------------------------------------------------------------------------------
# Relative Power Contribution
# ------------------------------------------------------------------------------

library(timsac)


y <- cbind(oil, gas)



# ----------
# relative power contribution
# max.order:  upper limit of model order

rpc <- mulnos(y, max.order = 20, h = 26)


names(rpc)


rpc$diffr[,,1:2]




# ----------

graphics.off()

par(mfrow = c(1,2))

plot(rpc$diffr[1,1,], type = "l", ylim = c(0, 1), col = "black", lwd = 2, main = "oil")
lines(1:27, rpc$diffr[1,2,], type = "l", col = "blue", lwd = 2)

plot(rpc$diffr[2,1,], type = "l", ylim = c(0, 1), col = "blue", lwd = 2, main = "gas")
lines(1:27, rpc$diffr[2,2,], type = "l", col = "black", lwd = 2)



# -->
# for oil, gas has some power at 3
# for gas, oil has large power at 1


