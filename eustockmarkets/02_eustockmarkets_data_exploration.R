setwd("//media//kswada//MyFiles//R//eustockmarkets")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  EuStockMarkets
# ------------------------------------------------------------------------------

data("EuStockMarkets", package = "datasets")


str(EuStockMarkets)

head(EuStockMarkets)



# ----------
dax <- EuStockMarkets[, "DAX"]




# ------------------------------------------------------------------------------
# data exploration:  plot time series
# ------------------------------------------------------------------------------

par(mfrow=c(2,1), mar = c(2,2,2,2))

plot(dax, type = "l", main = "original DAX")

plot(log(dax), type = "l", main = "logged DAX")




# ----------
forecast::ndiffs(dax)

forecast::ndiffs(log(dax))



# -->
# suggests that first difference of natural logarithm is stationary




# ------------------------------------------------------------------------------
# data exploration:  plot time series, log version
# ------------------------------------------------------------------------------

Rdax <- diff(log(dax))

par(mfrow=c(1,1), mar = c(2,2,2,2))
plot(Rdax, type = "l", main = "1st difference of logged DAX")



# kurtosis > 3 (6.27)
psych::describe(Rdax)



# ----------
# Histogram
MASS::truehist(Rdax, xlab = "1st difference of logged DAX", col = gray(.7))



# -->
# Typical time series pattern for returns.
# The mean of the returns is close to zero, which the variance (i.e. variance around the mean) appears to be different over time.
# There are some unusually large positive and negative observations, therefore kurtosis may be an issue in the returns.
# Since returns can take both negative and positive values, a continuous distribution on the real line is appropriate



