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



# ------------------------------------------------------------------------------
# data exploration:  Y vs. continuous X
# ------------------------------------------------------------------------------

# note that lowess requires na.excluded data

par(mfrow = c(2,2))

plot(weight ~ height, data = data2, ylab = "weight", cex.lab = 1.25, xlab = "height", pch = 20, col = gray(0.7))
lines(lowess(data2$height, data2$weight), col = "blue", lwd = 1)


plot(weight ~ repwt, data = data2, ylab = "weight", cex.lab = 1.25, xlab = "repwt", pch = 20, col = gray(0.7))
lines(lowess(data2$repwt, data2$weight), col = "blue", lwd = 1)


plot(weight ~ repht, data = data2, ylab = "weight", cex.lab = 1.25, xlab = "repht", pch = 20, col = gray(0.7))
lines(lowess(data2$repht, data2$weight), col = "blue", lwd = 1)





# ------------------------------------------------------------------------------
# data exploration:  Y vs. continuous X  by ggplot
# ------------------------------------------------------------------------------

# scale_y_log10(): plot the response and all other features on a log scale
# also plotting a confidence band around the smoothed curve, and adds a linear regression line

library(ggplot2)

gg <- ggplot(data, aes(height, weight)) + 
  stat_smooth(method = "loess", size = 2, fill = "blue", alpha = 0.25) +
  stat_smooth(method = "lm", color = "red", size = 1.25, se = FALSE) +
#  scale_y_log10(breaks = c(1, 2, 5, 10, 20)) +
  labs(y = "weight", x = "height")


gg



# by group
gg + facet_grid(~ sex)



