# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R/phillips5_aus")

packages <- c("dplyr", "AER", "lmtest", "stargazer", "dynlm", "broom", "knitr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  phillips5_aus
# ------------------------------------------------------------------------------
data("phillips5_aus", package = "POE5Rdata")

data <- phillips5_aus

glimpse(data)

str(data)

dim(data)


# ----------
# convert to ts object
is.ts(data)
data.ts <- ts(data[,c(2,3,4)], start = c(1987, 1), end = c(2016, 1), frequency = 4)



# ------------------------------------------------------------------------------
# Sample autocorrelation for unemployment by acf()
# ------------------------------------------------------------------------------
graphics.off()

acf(data.ts[,"u"], lag.max = 40, main = "Correlogram for Australian Unemployment Rate")

acf(data.ts[,"inf"], lag.max = 40, main = "Correlogram for Australian Inflation Rate")

acf(data.ts[,"du"], lag.max = 40, main = "Correlogram for Change in Australian Unemployment Rate")


# -->
# 1st difference of Australian Unemployment Rate still has autocorrelation at lag 1-3 and shows some cycle.



# ----------
# check by plot
plot.ts(data.ts[,"du"])



# ------------------------------------------------------------------------------
# Check order of integration
# ------------------------------------------------------------------------------

forecast::ndiffs(data.ts[,"u"])

forecast::ndiffs(data.ts[,"inf"])


# -->
# but order of integration of Australian Unemployment Rate is 1


