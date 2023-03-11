setwd("//media//kswada//MyFiles//R//airquality")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  airquality
# ------------------------------------------------------------------------------

data("airquality")


str(airquality)

car::some(airquality)


airquality2 <- na.exclude(airquality)



# ------------------------------------------------------------------------------
# data exploration:  Y vs. continuous X
# ------------------------------------------------------------------------------

par(mfrow = c(2,2))

plot(Ozone ~ Solar.R, data = airquality2, ylab = "airquality", cex.lab = 1.25, xlab = "Solar.R", pch = 20, col = gray(0.7))
lines(lowess(airquality2$Solar.R, airquality2$Ozone), col = "blue", lwd = 1)


plot(Ozone ~ Wind, data = airquality2, ylab = "airquality", cex.lab = 1.25, xlab = "Wind", pch = 20, col = gray(0.7))
lines(lowess(airquality2$Wind, airquality2$Ozone), col = "blue", lwd = 1)


plot(Ozone ~ Temp, data = airquality2, ylab = "airquality", cex.lab = 1.25, xlab = "Temp", pch = 20, col = gray(0.7))
lines(lowess(airquality2$Temp, airquality2$Ozone), col = "blue", lwd = 1)




# ------------------------------------------------------------------------------
# data exploration:  Y vs. continuous X  by ggplot
# ------------------------------------------------------------------------------

# scale_y_log10(): plot the response and all other features on a log scale
# also plotting a confidence band around the smoothed curve, and adds a linear regression line of log(articles) on mentor publications

library(ggplot2)

gg <- ggplot(airquality, aes(Solar.R, Ozone)) + 
  stat_smooth(method = "loess", size = 2, fill = "blue", alpha = 0.25) +
  stat_smooth(method = "lm", color = "red", size = 1.25, se = FALSE) +
  scale_y_log10(breaks = c(1, 2, 5, 10, 20)) +
  labs(y = "Ozone", x = "Solar.R")


gg <- ggplot(airquality, aes(Wind, Ozone)) + 
  stat_smooth(method = "loess", size = 2, fill = "blue", alpha = 0.25) +
  stat_smooth(method = "lm", color = "red", size = 1.25, se = FALSE) +
  scale_y_log10(breaks = c(1, 2, 5, 10, 20)) +
  labs(y = "Ozone", x = "Wind")


gg


