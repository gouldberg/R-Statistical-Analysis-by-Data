rm(list=ls())
# setwd("/media/kswada/MyFiles/R/msci_day")
setwd("/media/kswada/MyFiles/R/Econometrics/msci_day")


packages <- c("dplyr", "AER", "stargazer", "broom", "knitr", "tseries", "vars", "MTS", "forecast")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  msci_day
# ------------------------------------------------------------------------------

# data <- read.table("/media/kswada/MyFiles/references/経済・ファイナンスデータの計量時系列分析/msci_day.txt", header=T, stringsAsFactors=F)

data <- read.table("msci_day.txt", header=T, stringsAsFactors=F)

str(data)


car::some(data)


data <- data[,c(2:8)]



# ----------
# This data is daily and only has eigyo-bi,  so do not convert to ts type
graphics.off()
par(cex = 1.4, mar = c(4,4,2,1), lwd = 1.6, mfrow=c(1,1))

MTSplot(data)




# ------------------------------------------------------------------------------
# transform to stock return ration (log and diff) to secure stationarity
# ------------------------------------------------------------------------------

data_dif <- data.frame(ca = diff(log(data$ca)), fr = diff(log(data$fr)), ge = diff(log(data$ge)), it = diff(log(data$it)),
                       jp = diff(log(data$jp)), uk = diff(log(data$uk)), us = diff(log(data$us)))

MTSplot(data_dif)



# ----------
# note that the number of rows are decreased to 1390
nrow(data)

nrow(data_dif)




# ------------------------------------------------------------------------------
# Check Order of Integration
#   - ndiff() uses a unit root test and estimate the number of differences required to make a given time series stationary.
#     The KPSS, Augmented Dickey-Fuller test or Phillips-Perron test is used.
# ------------------------------------------------------------------------------

apply(data, MARGIN=2, forecast::ndiffs)

apply(data_dif, MARGIN=2, forecast::ndiffs)



# -->
# data_dif is stationary.



# ------------------------------------------------------------------------------
# Correlogram to study the stationarity properties of a time series
#  - Nonstationary series often display serial correlations for many lags
# ------------------------------------------------------------------------------

library(forecast)


# ----------
# Nonstationary series
graphics.off()
par(cex = 1.4, mar = c(4,4,4,4), lwd = 1.6, mfrow=c(3,3))
apply(data, 2, Acf)


graphics.off()
par(cex = 1.4, mar = c(4,4,4,4), lwd = 1.6, mfrow=c(3,3))
apply(data, 2, Pacf)



# ----------
# Stationary series
graphics.off()
par(cex = 1.4, mar = c(4,4,4,4), lwd = 1.6, mfrow=c(3,3))
apply(data_dif, 2, Acf)


graphics.off()
par(cex = 1.4, mar = c(4,4,4,4), lwd = 1.6, mfrow=c(3,3))
apply(data_dif, 2, Pacf)

