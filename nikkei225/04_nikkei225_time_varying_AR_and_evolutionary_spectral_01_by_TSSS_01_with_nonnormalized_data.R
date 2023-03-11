setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\nikkei225")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  nikkei225
# ------------------------------------------------------------------------------


dat <- read.table("nikkei225.txt", sep = "", header = T, colClasses = "numeric")


head(dat)


dat <- dat$nikkei225


dat <- diff(log(dat))




# ------------------------------------------------------------------------------
# Time Varying Coefficients AR Model by TSSS
# ------------------------------------------------------------------------------

library(TSSS)


# span: local stationary span
# outlier:  positions of outliers
# tau2.ini:  initial estimate of variance of the system noise tau^2
# delta:  search width


# trying trend.order = 1 and 2 + AR order 1 to 20


for(k in 1:2){
  for(m in 1:20){
    z <- tvar(dat, trend.order = k, ar.order = m, span = 40, plot = FALSE)

    print(paste0("AR order: ", m, "  k: ", k, " AIC: ", z$aic))
  }
}



# -->
# Showing best model can not be estimated ...
# many lags are required



