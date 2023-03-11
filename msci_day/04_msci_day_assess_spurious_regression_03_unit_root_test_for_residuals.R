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




# ------------------------------------------------------------------------------
# Unit Root Test (KPSS) for residuals
# ------------------------------------------------------------------------------

# Unit Root Test (KPSS):  rejected  --> indicating that residuals are unit root --> Spurious Regression !!!
# Null hypothesis of KPSS test:  an observable time series is stationary around a deterministic trend against the alternative of a unit root
# urca::ur.kpss(): the test types specify as deterministic component either a constant "mu" or a constant with linear trend "tau"

kpsstest_rn <- list()

for(i in 1:length(mod)){
  kpsstest_rn[[i]] <- summary(urca::ur.kpss(resid(mod[[i]]), type ="mu", lags = "long"))
}


kpsstest_rn

