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



# ------------------------------------------------------------------------------
# data exploration:  time series plot
# ------------------------------------------------------------------------------

library(MTS)


# AL(Alabama), AK(Alaska), AZ(Arizona), AR(Arkansas), CA(California)

MTSplot(da[,1:5])



# diffM takes difference from data.frame
MTSplot(diffM(da[,1:5]))




# ----------
# all time series plot

par(mfrow = c(1,1), mar = c(2,2,2,2))

matplot(diffM(da), type = "l")



# -->
# The series are highly related and still contain some aberrant observations



# ----------
library(forecast)


apply(da, 2, ndiffs)


apply(diffM(da), 2, ndiffs)


