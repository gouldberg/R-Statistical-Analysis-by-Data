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
# data exploration:  Y vs. continuous X  by ggplot by group
# ------------------------------------------------------------------------------


# scale_y_log10(): plot the response and all other features on a log scale


library(ggplot2)

gg <- ggplot(airquality, aes(Solar.R, Ozone)) + 
  stat_smooth(method = "loess", size = 2, fill = "blue", alpha = 0.25) +
  stat_smooth(method = "lm", color = "red", size = 1.25, se = FALSE) +
#  scale_y_log10(breaks = c(1, 2, 5, 10, 20)) +
  labs(y = "Ozone", x = "Solar.R")


gg <- ggplot(airquality, aes(Wind, Ozone)) + 
  stat_smooth(method = "loess", size = 2, fill = "blue", alpha = 0.25) +
  stat_smooth(method = "lm", color = "red", size = 1.25, se = FALSE) +
#  scale_y_log10(breaks = c(1, 2, 5, 10, 20)) +
  labs(y = "Ozone", x = "Wind")


gg <- ggplot(airquality, aes(Temp, Ozone)) + 
  stat_smooth(method = "loess", size = 2, fill = "blue", alpha = 0.25) +
  stat_smooth(method = "lm", color = "red", size = 1.25, se = FALSE) +
  #  scale_y_log10(breaks = c(1, 2, 5, 10, 20)) +
  labs(y = "Ozone", x = "Temp")


gg + facet_wrap(~ Month)



# ------------------------------------------------------------------------------
# data exploration:  Y vs. continuous X  by group
# ------------------------------------------------------------------------------

formula = Ozone ~ Temp | Month

coplot(formula, data = airquality, ylab = "Ozone", xlab = "Temp", las=1)




# ------------------------------------------------------------------------------
# data exploration:  Y vs. continuous X  by group
# ------------------------------------------------------------------------------

par(mfrow = c(1,2))

xyplot(Ozone ~ Wind | Month, data = airquality, cex = 0.8, col = "black", type = c("p", "g", "smooth"))



# -->
# Very much different !!!  (especially in June and September)
# Wind is strong in May and September, and June



# ----------
xyplot(Ozone ~ Solar.R | Month, data = airquality, cex = 0.8, col = "black", type = c("p", "g", "smooth"))



# -->
# Very much different !!!  (especially in June)



# ----------
xyplot(Ozone ~ Temp | Month, data = airquality, cex = 0.8, col = "black", type = c("p", "g", "smooth"))

xyplot(Ozone ~ Temp, data = airquality, cex = 0.8, col = "black", type = c("p", "g", "smooth"))



# -->
# In May and September, at low temperature, the variation is large



# ----------
xyplot(Wind ~ Solar.R | Month, data = airquality, cex = 0.8, col = "black", type = c("p", "g", "smooth"))

