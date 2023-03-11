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
# data exploration:  time series plot
# ------------------------------------------------------------------------------


# This data is daily and only has eigyo-bi,  so do not convert to ts type
graphics.off()

par(cex = 1.4, mar = c(4,4,2,1), lwd = 1.6, mfrow=c(1,1))

MTSplot(data)



# ----------
# take log
data <- log(data)

MTSplot(data)


