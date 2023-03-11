rm(list = ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\05_anomaly_detection\\simdata3")




# ------------------------------------------------------------------------------
# data:  generation
# ------------------------------------------------------------------------------

library(densratio)


# ----------
set.seed(1234)


# normal operation data
x1 <- rnorm(400, mean = 1, sd = 0.3)


# test data:  bit different data and abnormal data (only 50 points)
x2_1 <- c(rnorm(23, mean = 0.8, sd = 0.3), 3)
x2_2 <- c(rnorm(23, mean = 1.2, sd = 0.3), 3.3)


# test data:  bit different data
x3_1 <- c(rnorm(25, mean = 0.8, sd = 0.3))
x3_2 <- c(rnorm(25, mean = 1.2, sd = 0.3))
x3 <- c(x2_1, x2_2)



# test data = normal operation data
x4 <- rnorm(50, mean = 1, sd = 0.3)



# ----------
graphics.off()

par(mfrow = c(2,1))

hist(x1, breaks = seq(-5, 6, 0.01))

hist(x2, breaks = seq(-5, 6, 0.01))




# ------------------------------------------------------------------------------
# Estimate Density Ratio and anomaly scores
# ------------------------------------------------------------------------------

# function of estimate density ratio
densratio_obj2 <- densratio(x1, x2)

densratio_obj3 <- densratio(x1, x3)

densratio_obj4 <- densratio(x1, x4)


# sigma:  band width
densratio_obj2$kernel_info$kernel
densratio_obj2$kernel_info$kernel_num
densratio_obj2$kernel_info$sigma



# ----------
# estimate density ratio

new_x <- seq(-5, 6, by = 0.01)

estimated_density_ratio2 <- densratio_obj2$compute_density_ratio(new_x)

estimated_density_ratio3 <- densratio_obj3$compute_density_ratio(new_x)

estimated_density_ratio4 <- densratio_obj4$compute_density_ratio(new_x)


# 2 and 3 are different
estimated_density_ratio2 == estimated_density_ratio3




# ----------
# Compute Anomaly Score
a2 <- - log(estimated_density_ratio2 + 0.0001)

a3 <- - log(estimated_density_ratio3 + 0.0001)

a4 <- - log(estimated_density_ratio4 + 0.0001)


# -->
# the smaller the density ratio  -->  anomaly
# the larger the anomaly score --> anomaly




# ----------
graphics.off()

par(mfcol = c(4,3))

hist(x1, breaks = seq(-5, 6, 0.01), main = "normal operation data")

hist(x2, breaks = seq(-5, 6, 0.01), main = "test data")

plot(new_x, pch = 19, estimated_density_ratio2, main = "density ratio", ylim = c(0, 1))

plot(new_x, a2, pch = 19, main = "anomaly score", ylim = c(0, 10))
abline(h = 1, lty = 1)



hist(x1, breaks = seq(-5, 6, 0.01), main = "normal operation data")

hist(x3, breaks = seq(-5, 6, 0.01), main = "test data 2")

plot(new_x, pch = 19, estimated_density_ratio3, main = "density ratio", ylim = c(0, 1))

plot(new_x, a3, pch = 19, main = "anomaly score", ylim = c(0, 10))
abline(h = 1, lty = 1)



hist(x1, breaks = seq(-5, 6, 0.01), main = "normal operation data")

hist(x4, breaks = seq(-5, 6, 0.01), main = "test data 3")

plot(new_x, pch = 19, estimated_density_ratio4, main = "density ratio", ylim = c(0, 1))

plot(new_x, a4, pch = 19, main = "anomaly score", ylim = c(0, 10))
abline(h = 1, lty = 1)

