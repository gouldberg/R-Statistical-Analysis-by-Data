rm(list = ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\05_anomaly_detection\\davis")



# ------------------------------------------------------------------------------
# data:  Davis
# ------------------------------------------------------------------------------


data <- read.csv("Davis.txt", header = T, sep = "\t")


str(data)


car::some(data)




# ------------------------------------------------------------------------------
# Anomaly Detection for the data assumed to following non-symmetric distribution
# ------------------------------------------------------------------------------

summary(data$weight)


hist(data$weight, breaks = seq(30, 170, by = 5))


# data <- data %>% filter(weight < 150)



# ----------
# sample mean and standard distribution

N <- length(data$weight)


mu <- mean(data$weight)


si <- sd(data$weight) * sqrt((N - 1) / N)

c(mu, si)




# ----------
# moments method: 1st moment and 2nd moment

kmo <- (mu / si)^2


smo <- si^2 / mu




# ----------
# estimate parameter of gamma distribution by maximum likelihood method

ml <- MASS::fitdistr(data$weight, "gamma")


kml <- ml$estimate["shape"]


sml <- 1 / ml$estimate["rate"]




# ----------
c(kmo, smo)


c(kml, sml)




# ----------

graphics.off()

par(mfrow = c(1,1))


plot(curve(dgamma(x, shape = kml, scale = sml), 30, 170), col = "blue", type = "l")

hist(data$weight, breaks = seq(30, 170, by = 5))




# ----------
# anomaly score

graphics.off()


a <- data$weight / smo - (kmo - 1) * log(data$weight / smo)


# 1% percential point
th <- order(a, decreasing = T)[0.01 * N]


plot(a, xlab = "index", ylab = "anomaly score")

lines(0:nrow(data), rep(a[th], length(0:nrow(data))), col = "red", lty = 2)



