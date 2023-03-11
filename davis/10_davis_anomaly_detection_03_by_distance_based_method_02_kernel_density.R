rm(list = ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\05_anomaly_detection\\01_異常検知\\davis")


# ------------------------------------------------------------------------------
# data:  Davis
# ------------------------------------------------------------------------------


data <- read.csv("Davis.txt", header = T, sep = "\t")


str(data)


car::some(data)




# ------------------------------------------------------------------------------
# Anomaly Detection based on distance metric
# Estimate Kernel Density
# ------------------------------------------------------------------------------

# KernSmooth is able to estimate Kernel Density up to 2 dimension

library(KernSmooth)


X <- data[, c("weight", "height")]


# select a bandwidth for Kernel Density estimation
( h <- c(dpik(X$weight), dpik(X$height)) )




# ----------
# 2-dimenational kernel density estimation
est <- bkde2D(X, bandwidth = h, gridsize = c(10^3, 10^3))

names(est)


# fhat:  matrix of density estimates over the mesh induced by x1 and x2

d <- list(x = est$x1, y = est$x2, z = est$fhat)




# ----------
image(d, col = terrain.colors(7), xlim = c(35, 110), ylim = c(145, 200))

contour(d, add = T)





# ------------------------------------------------------------------------------
# Anomaly Detection based on distance metric
# Compute anomaly score
# ------------------------------------------------------------------------------


# Kernel Matrix: K

M <- 2

dis1 <- (as.matrix(dist(X[,1])) / h[1])^2

dis2 <- (as.matrix(dist(X[,2])) / h[2])^2

dis <- dis1 + dis2

dim(dis)

K <- prod(h)^(-0.5) / (2 * pi)^(M / 2) * exp( -0.5 * dis )

# K <- est$fhat
# K <- as.kernelMatrix(crossprod(t(X)))



# ----------
n <- nrow(X)


# removing self data
aa <- colSums(K) - diag(K)


# set lower limit
lowerLim <- 10^(-20)

aa[(aa < lowerLim)] <- lowerLim



# ----------
# anomaly score

a <- (-1) * log(aa / (n - 1))




# ------------------------------------------------------------------------------
# plot anomaly scores and anoamly dataCompute anomaly score
# ------------------------------------------------------------------------------


graphics.off()

plot(a, xlab = "sample ID", ylab = "anomaly score")



# ----------
# anomaly points

( idx <- which(a > 10) )

colorcode <- rep(0, length(a))

colorcode[idx] <- 1




# ----------
# plot on kernel density

graphics.off()

image(d, col = terrain.colors(7), xlim = c(30, 170), ylim = c(50, 200))

contour(d, add = T)

points(X, pch = 21, bg = colorcode)


