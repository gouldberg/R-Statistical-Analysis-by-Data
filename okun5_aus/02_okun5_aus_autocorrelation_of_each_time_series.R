# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R/okun5_aus")

packages <- c("dplyr", "AER", "lmtest", "stargazer", "dynlm", "broom", "knitr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  okun5_aus
# ------------------------------------------------------------------------------
data("okun5_aus", package = "POE5Rdata")

glimpse(okun5_aus)

str(okun5_aus)


# remove dateid01
data <- okun5_aus[, c(2,3)]



# ----------
# convert to ts object
is.ts(data)

data.ts <- ts(data, start = c(1978, 2), end = c(2016, 2), frequency = 4)



# ------------------------------------------------------------------------------
# Sample autocorrelation for unemployment by acf()
# ------------------------------------------------------------------------------
graphics.off()

acf(data.ts[,"u"], lag.max = 40, main = "Correlogram for Australian Unemployment Rate")

acf(data.ts[,"g"], lag.max = 40, main = "Correlogram for Australian Growth Rate")

acf(diff(data.ts[,"u"]), lag.max = 40, main = "Correlogram for Difference in Australian Unemployment Rate")


# -->
# 1st difference of Australian Unemployment Rate stiall has autocorrelation at lag 1-3 and shows some cycle.



# ----------
# check by plot
plot.ts(diff(data.ts[,"u"]))



# ------------------------------------------------------------------------------
# Check order of integration
# ------------------------------------------------------------------------------

forecast::ndiffs(data.ts[,"u"])

forecast::ndiffs(data.ts[,"g"])


# -->
# but order of integration of Australian Unemployment is 1


