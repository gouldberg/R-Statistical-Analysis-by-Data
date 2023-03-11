rm(list = ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\regression_basics\\davis")


# ------------------------------------------------------------------------------
# data:  Davis
# ------------------------------------------------------------------------------

data <- read.csv("Davis.txt", header = T, sep = "\t")


str(data)


car::some(data)


data2 <- na.exclude(data)




# ------------------------------------------------------------------------------
# single variable simple linear regression
# ------------------------------------------------------------------------------

linmod <- lm(weight ~ repwt, data = data2)


summary(linmod)


coef(linmod)


# degrees of freedom:
# 181 - (1 + 1)



# ----------
# goodness of fit (GOF) = Multiple R-squared = 0.6979
# adjusted:  0.6962




# ----------
graphics.off()

plot(weight ~ repwt, data = data2)

abline(linmod)
