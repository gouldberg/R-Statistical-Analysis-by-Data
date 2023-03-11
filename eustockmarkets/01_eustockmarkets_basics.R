setwd("//media//kswada//MyFiles//R//eustockmarkets")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  EuStockMarkets
#   - DAX returns data from 1991 to 1998.
#     The original DAX index is one of the four financial indices given in this dataset.
#     Returns are calculated as the first difference of the natural logarithm of the series
#   - Variables:
#        - DAX:  Germany DAX index
#        - SMI:  Switzerland SMI index
#        - CAC:  France CAC index
#        - FTSE:  UK FTSE index
# ------------------------------------------------------------------------------

data("EuStockMarkets", package = "datasets")


str(EuStockMarkets)

head(EuStockMarkets)



# ----------
dax <- EuStockMarkets[, "DAX"]




# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

par(mfrow=c(1,2), mar = c(2,2,2,2))
plot(dax, type = "l", main = "original DAX")
plot(log(dax), type = "l", main = "logged DAX")



# ----------
forecast::ndiffs(dax)
forecast::ndiffs(log(dax))


# -->
# suggests that first difference of natural logarithm is stationary



# ----------
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



