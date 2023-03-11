grasetwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\gassendout")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Gas Sendout
# ------------------------------------------------------------------------------

gas <- read.table("GasSendout.txt", sep = "", header = F, colClasses = "numeric")



colnames(gas) <- c("MetI", "gasSendout", "V3")


head(gas)




# ------------------------------------------------------------------------------
# preprocessing for gasSendout
# ------------------------------------------------------------------------------


# removing annual trend

output_yr_comp <- smooth.spline(time(gas$gasSendout), gas$gasSendout, spar = 1.0)$y


output_a <- gas$gasSendout - output_yr_comp




# set frequency 7

output7 <- ts(as.numeric(output_a), frequency = 7)




# ----------
output_stl7 <- stl(output7, s.window = "per")

plot(output_stl7)



# weekly cycle component
output_wk_comp <- c(output_stl7$time.series[,"seasonal"])




# ----------
# removing also weekly cycle component gasSendout

output <- output_a - output_wk_comp



graphics.off()

par(mfrow = c(2,2))

plot(gas$gasSendout, type = "l")

plot(output_yr_comp, type = "l")

plot(output_wk_comp, type = "l")

plot(output, type = "l")




# ------------------------------------------------------------------------------
# preprocessing for MetI
# ------------------------------------------------------------------------------


# removing annual trend

input_yr_comp <- smooth.spline(time(gas$MetI), gas$MetI, spar = 1.0)$y



# ----------
# removing also weekly cycle component gasSendout

input <- gas$MetI - input_yr_comp



graphics.off()

par(mfrow = c(2,2))

plot(gas$MetI, type = "l")

plot(input_yr_comp, type = "l")

plot(input, type = "l")





# ----------
# corrected gasSendout and MetI

graphics.off()

par(mfrow = c(2,1))

plot(output, type = "l")
abline(h = 0, v = seq(0, 400, by = 20), col = "gray", lty = 2)

plot(input, type = "l")
abline(h = 0, v = seq(0, 400, by = 20), col = "gray", lty = 2)



