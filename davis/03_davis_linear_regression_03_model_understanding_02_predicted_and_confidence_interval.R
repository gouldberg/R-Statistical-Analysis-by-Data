rm(list = ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\regression_basics")



# ------------------------------------------------------------------------------
# data:  Davis
# ------------------------------------------------------------------------------

data <- read.csv("Davis.txt", header = T, sep = "\t")


str(data)


car::some(data)


data2 <- na.exclude(data)



# ----------
linmod <- lm(weight ~ repwt, data = data2)





# ------------------------------------------------------------------------------
# predicted and confidential interval
# ------------------------------------------------------------------------------

pred <- predict(linmod, data = data2, se = TRUE, conf.level = 0.95)

ord <- order(data2$repwt)



# ----------
graphics.off()

plot(weight ~ repwt, data = data2)

abline(linmod)

lines(pred$fit[ord] + pred$se[ord] ~ repwt[ord], data = data2, lty = 2, col = "blue")
lines(pred$fit[ord] - pred$se[ord] ~ repwt[ord], data = data2, lty = 2, col = "blue")


