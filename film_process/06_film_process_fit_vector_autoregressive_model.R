setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\film_process")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Film Extrusion Process
# ------------------------------------------------------------------------------


film <- read.table("FilmProcess.txt", sep = "", header = F, colClasses = "numeric")


head(film)



# ----------
hc <- film$V1

vp <- film$V2

prs <- film$V3

wid <- film$V4




# ------------------------------------------------------------------------------
# Fit Vector AR model (VAR)
#   - We might envision dynamc relations among the three series defined as the first order relation,
#     which expresses the current value of mortality as a linear combination of trend and its immediate past value and the past values of
#     temperature and particulate levels.
#     Similarly, dependence of temperature and particulate levels on the other series.
# ------------------------------------------------------------------------------


# vars Fit vector AR models via least squares.
library(vars)



x <- cbind(prs, hc, vp)




# ----------
# "both" fits constant + trend
# only p = 1

summary(VAR(x, p = 1, type = "both"))






# ------------------------------------------------------------------------------
# Select VAR model by VARselect
# ------------------------------------------------------------------------------

# "both" fits constant + trend

VARselect(x, lag.max = 10, type = "both")




# -->
# here we select p = 2



# ------------------------------------------------------------------------------
# Fit VAR model by p = 2
# ------------------------------------------------------------------------------

summary(fit_var <- VAR(x, p = 2, type = "both"))





# ----------
# Acf2 can not be used for multivariate time series ...

acf(resid(fit_var), lag.max = 52)


acf(resid(fit_var), 52)$acf




# ------------------------------------------------------------------------------
# Examine the multivariate version of the Q-test
# ------------------------------------------------------------------------------

vars::serial.test(fit_var, lags.pt = 12, type = "PT.adjusted")



# -->
# Thus, unexpectedly, the Q-test rejects the null hypothesis that the noise is white.

