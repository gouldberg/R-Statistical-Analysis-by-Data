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
# Anomaly Detection for the data assumed to following normal distribution
# ------------------------------------------------------------------------------

summary(data$weight)


hist(data$weight, breaks = seq(30, 170, by = 5))


# data <- data %>% filter(weight < 150)



# ----------
# sample mean and variance

mu <- mean(data$weight)


s2 <- mean((data$weight - mu)^2)


c(mu, s2)




# ----------
# anomaly score

graphics.off()

a <- round((data$weight - mu) ^ 2 / s2, 4)


# X^2 squared distribution (df = 1), 1% level threshold
th <- qchisq(0.99, 1)


plot(a, xlab = "index", ylab = "anomaly score")

lines(0:nrow(data), rep(th, length(0:nrow(data))), col = "red", lty = 2)




# ------------------------------------------------------------------------------
# Anomaly Detection for the data assumed to following bivariate normal distribution
# 
# --> MORE CLEARLY DETECTED !!!
# ------------------------------------------------------------------------------

# weight and height

X <- cbind(data$weight, data$height)

plot(X[,1], X[,2], pch = 16, xlab = "weight", ylab = "height")




# ----------
mx <- colMeans(X)

( Xc <- X - matrix(1, nrow(X), 1) %*% mx )


Sx <- t(Xc) %*% Xc / nrow(X)

a <- rowSums((Xc %*% solve(Sx)) * Xc)

plot(a, xlab = "index", ylab = "anomaly score")

lines(0:nrow(data), rep(th, length(0:nrow(data))), col = "red", lty = 2)



