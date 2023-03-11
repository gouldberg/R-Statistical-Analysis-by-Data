# setwd("//media//kswada//MyFiles//R//email")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\m_tenstocks")


packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  m-tenstocks
# ------------------------------------------------------------------------------

mtenstocks <- read.csv("m-tenstocks.txt", sep = " ", header = T)


str(mtenstocks)


dim(mtenstocks)


car::some(mtenstocks)



# ----------
# log returns

rtn <- log(mtenstocks[,2:11] + 1)




# ------------------------------------------------------------------------------
# Correlation analysis:  autocorrelation and partial auto-correlation
# ------------------------------------------------------------------------------

library(astsa)


graphics.off()

# for(i in 2:11){ acf2(mtenstocks[,i], max.lag = 24, main = paste0(colnames(mtenstocks)[i])) }

for(i in 1:10){ acf2(rtn[,i], max.lag = 24, main = paste0(colnames(rtn)[i])) }



# -->
# all time series can be considered as stationary

