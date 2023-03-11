rm(list=ls())
# setwd("/media/kswada/MyFiles/R/msci_day")
setwd("/media/kswada/MyFiles/R/Econometrics/msci_day")


packages <- c("dplyr", "AER", "stargazer", "broom", "knitr", "tseries", "vars", "MTS", "forecast")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  msci_day
# ------------------------------------------------------------------------------

data <- read.table("msci_day.txt", header=T, stringsAsFactors=F)

str(data)


car::some(data)



# ----------
# only jp, fr and ca
# NOTE that we order by jp --> uk --> us
data <- data[,c("jp","uk","us")]



# ----------
# This data is daily and only has eigyo-bi,  so do not convert to ts type
graphics.off()
par(cex = 1.4, mar = c(4,4,2,1), lwd = 1.6, mfrow=c(1,1))
MTSplot(data)



# ----------
# transform to stock return ration (log and diff) to secure stationarity
data_dif <- data.frame(jp = diff(log(data$jp)), uk = diff(log(data$uk)), us = diff(log(data$us)))
MTSplot(data_dif)



# ----------
# note that the number of rows are decreased to 1390
nrow(data)
nrow(data_dif)



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
# Note that refined model has lower AIC

