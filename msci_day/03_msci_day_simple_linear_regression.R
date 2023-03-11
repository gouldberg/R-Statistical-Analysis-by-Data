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




# ------------------------------------------------------------------------------
# simple linear regression
# ------------------------------------------------------------------------------


lmod_jp <- lm(jp ~ time(Date), data = data)


lmod_us <- lm(us ~ time(Date), data = data)



# ----------
summary(lmod_jp)


summary(lmod_us)




# ----------

car::residualPlots(lmod_jp)

car::residualPlots(lmod_us)



# ----------
par(mfrow = c(1,2))

plot(jp ~ time(Date), data, pch = 20, col = "gray", cex = 0.2)

abline(lmod_jp)

plot(us ~ time(Date), data, pch = 20, col = "gray", cex = 0.2)

abline(lmod_us)


