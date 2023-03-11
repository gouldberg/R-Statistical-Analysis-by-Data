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
# Regression with lagged variables:  impulse response function
# ------------------------------------------------------------------------------


# L:  degree of smoothing
# M:  must be even, number of terms used in the lagged regression, abs(t) >= M/2
# threshold:  the cut-off used to set small (in absolute value) regression coefficients equal to zero


mod_itoo <- astsa::LagReg(input = input, output = output, L = 15, M = 20, threshold = 0.001)


# by original series
# mod_itoo <- astsa::LagReg(input = gas$MetI, output = gas$gasSendout, L = 3, M = 20, threshold = 0.001)





# ------------------------------------------------------------------------------
# Regression with lagged variables:  impulse response function for inverse relationship
# ------------------------------------------------------------------------------

mod_otoi <- astsa::LagReg(input = output, output = input, L = 15, M = 20, inverse = TRUE,  threshold = 0.001)




# ------------------------------------------------------------------------------
# Lagged regression again by parsimonious model
# ------------------------------------------------------------------------------

input <- ts(input)

output <- ts(output)


gas_m <- ts.intersect(gass = output, 
                    meti0 = stats::lag(input, 0),
                    meti1 = stats::lag(input, 1),
                    meti3 = stats::lag(input, 3),
                    meti5 = stats::lag(input, 5),
                    meti7 = stats::lag(input, 7),
                    meti9 = stats::lag(input, 9), dframe = TRUE)
                    


( u <- lm(gass ~ meti0 + meti1 + meti3 + meti5 + meti7 + meti9, data = gas_m) )

( u <- lm(gass ~ meti0, data = gas_m) )


summary(u)



coef(u)




# ----------
acf2(resid(u))




# ------------------------------------------------------------------------------
# with autocorrelated errors
# ------------------------------------------------------------------------------


sarima_mod <- sarima(gas_m[,1], p = 1, d = 0, q = 6, xreg = gas_m[,2:7])


output_fit <- output + resid(sarima_mod$fit)


output_final_fit <- output_fit + output_wk_comp[1:356] + output_yr_comp[1:356]




# ----------
par(mfrow = c(1,1))


plot(gas$gasSendout, type = "l")

lines(time(gas$gasSendout), c(output_final_fit, rep(NA,9)), type = "l", col = "blue")




# ----------
par(mfrow = c(1,2))

res <- gas$gasSendout[1:356] - output_final_fit

plot(res, type = "h")



# ----------
qqnorm(res)

qqline(res)
