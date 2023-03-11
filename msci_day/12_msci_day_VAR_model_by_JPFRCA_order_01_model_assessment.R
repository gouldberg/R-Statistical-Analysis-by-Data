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



# ----------
# only jp, fr and ca
# NOTE that we order by jp --> fr --> ca
data <- data[,c("jp","fr","ca")]



# ----------
# This data is daily and only has eigyo-bi,  so do not convert to ts type
graphics.off()
par(cex = 1.4, mar = c(4,4,2,1), lwd = 1.6, mfrow=c(1,1))
MTSplot(data)



# ----------
# transform to stock return ration (log and diff) to secure stationarity
data_dif <- data.frame(jp = diff(log(data$jp)), fr = diff(log(data$fr)), ca = diff(log(data$ca)))
MTSplot(data_dif)



# ----------
# note that the number of rows are decreased to 1390
nrow(data)
nrow(data_dif)



# ------------------------------------------------------------------------------
# Vector Autoregression Model (VAR):  determine optimal lag length
# ------------------------------------------------------------------------------
library(vars)

varmat <- as.matrix(data_dif)


# ----------
# determine optimal lag length according to AIC, BIC, HQ, SC, FPE(final prediction error) of an empirical VAR(p) process
VARselect(varmat, lag.max = 10, type = "const")



# ----------
# determine optimal lag length according to AIC, BIC, HQ
MTS::VARorder(data_dif)



# -->
# we choose p = 2



# ------------------------------------------------------------------------------
# Vector Autoregression Model (VAR)
# ------------------------------------------------------------------------------
# Check the time series order:  we order by jp --> fr --> ca
names(data_dif)

varfit <- vars::VAR(varmat, p = 2, type = "const")

summary(varfit)



# -->
# NOTE that CA is quite independent and JP is dependent
# FR impacts to JP !!!



# ----------
varfit_MTS <- MTS::VAR(cbind(data_dif), p = 2)



# ------------------------------------------------------------------------------
# Model Simplification (Refinement)
# ------------------------------------------------------------------------------
# refVAR carry out model simplification of a fitted VAR model.
# The command uses a threshold to select target parameters for removal and computes information criteria of the simplified model for validation.
# The default threshold is 1.00
# The command also allows the user to specify zero parameters with a subcommand fixed.
varfit_MTS_ref <- refVAR(varfit_MTS, thres = 1.96)


varfit_MTS$aic;  varfit_MTS$bic;
varfit_MTS_ref$aic;  varfit_MTS_ref$bic;


# -->
# Note that the model is not refined much ...



# ------------------------------------------------------------------------------
# Model Checking
# ------------------------------------------------------------------------------
# fit and residuals, ACF residuals, PACF residuals
graphics.off()
plot(varfit)



# ----------
# Multivariate Pormanteau statistics, adjusted by degrees of freedom
# AR(p) and k is number of vectors (time-series)
# If not varfit_MTS but varfit_MTS_ref, decrease degrees of freedom by zero parameters
p <- 2;  k <- 3;
mq(varfit_MTS$residuals, adj = p * k^2)



# MTSdiag checks 24 lags of cross-correlation matrix in default
# p-value plot of individual ccm
# plot of p-values of the Qk(m) statistics
# residual plots
# If not varfit_MTS but varfit_MTS_ref, decrease degrees of freedom by zero parameters
MTSdiag(varfit_MTS, adj = p * k^2)



# check stability: stable model has all roots < 1
vars::roots(varfit)



