# setwd("//media//kswada//MyFiles//R//email")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\m_unempstatesAdj")


packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  m-unempstatesAdj
# ------------------------------------------------------------------------------

da <- read.csv("m-unempstatesAdj.txt", sep = "", header = T)


str(da)


dim(da)


car::some(da)



# ----------
# first difference

drate <- diffM(da)




# ------------------------------------------------------------------------------
# Correlation analysis:  autocorrelation and partial auto-correlation
# ------------------------------------------------------------------------------

library(astsa)


graphics.off()

for(i in 1:5){ acf2(drate[,i], max.lag = 24, main = paste0(colnames(drate)[i])) }



# -->
# Alabama: ARIMA(1,1,9)

sarima(da[,1], 1, 1, 9)


