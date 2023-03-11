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
# Vector Autoregression Model (VAR)
# ------------------------------------------------------------------------------

# Check the time series order:  we order by jp --> uk --> us
names(data_dif)

varfit <- vars::VAR(varmat, p = 3, type = "const")

summary(varfit)



# -->
# NOTE that US is quite independent and JP is dependent



# ----------
varfit_MTS <- MTS::VAR(cbind(data_dif), p = 3)


varfit_MTS



