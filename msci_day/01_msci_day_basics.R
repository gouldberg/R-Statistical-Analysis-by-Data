rm(list=ls())
# setwd("/media/kswada/MyFiles/R/msci_day")
# setwd("/media/kswada/MyFiles/R/Econometrics/msci_day")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\msci_day")

packages <- c("dplyr", "AER", "stargazer", "broom", "knitr", "tseries", "vars", "MTS", "forecast")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  msci_day
#   - daily (only eigyou-bi) MSCI (Morgan Stanley Capital International) data of G7 countries over the period of Jan. 2003 through Apr. 2008
#   - MSCI is widely used stock index since it covers many countries and industries 
# ------------------------------------------------------------------------------

data <- read.table("msci_day.txt", header=T, stringsAsFactors=F)

str(data)


car::some(data)


data <- data[,c(2:8)]



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
