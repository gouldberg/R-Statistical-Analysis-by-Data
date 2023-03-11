rm(list=ls())
# setwd("/media/kswada/MyFiles/R/msci_day")
setwd("/media/kswada/MyFiles/R/Econometrics/msci_day")


packages <- c("dplyr", "AER", "stargazer", "broom", "knitr", "tseries", "vars", "MTS", "forecast")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  msci_day
# ------------------------------------------------------------------------------

# data <- read.table("/media/kswada/MyFiles/references/経済�?�ファイナンス�?ータの計量時系列�??�?/msci_day.txt", header=T, stringsAsFactors=F)

data <- read.table("msci_day.txt", header=T, stringsAsFactors=F)

str(data)


car::some(data)


data <- data[,c(2:8)]



# ------------------------------------------------------------------------------
# data exploration
# ------------------------------------------------------------------------------

summary(data)


psych::describe(data)




# ----------
par(mfrow = c(1,3))
MASS::truehist(data$jp, nbins = 30, main = "JP")
MASS::truehist(data$uk, nbins = 30, main = "UK")
MASS::truehist(data$us, nbins = 30, main = "US")
