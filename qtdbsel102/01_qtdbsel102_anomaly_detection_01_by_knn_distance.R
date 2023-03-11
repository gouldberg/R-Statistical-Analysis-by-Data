rm(list = ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\05_anomaly_detection\\02_異常検知_時系列\\qtdbsel102")



# ------------------------------------------------------------------------------
# data:  qtdbsel102
# ------------------------------------------------------------------------------


data <- read.csv(file = "qtdbsel102.txt", header = FALSE, sep = "")



str(data)


head(data)





# ------------------------------------------------------------------------------
# data exploration:  time series plot
# ------------------------------------------------------------------------------


MTS::MTSplot(data)



# -->
# now we focus on 2nd series




# ------------------------------------------------------------------------------
# Vectorize time series by slide window
# ------------------------------------------------------------------------------


# window length: try 200, 100 ,10
w <- 100
# w <- 10
# w <- 200



# ----------
# training data set: Xtr

Xtr <- data[1:3000, 2]


# embedding time series
Dtr <- embed(Xtr, w)


dim(Dtr)



# both are same
Xtr[100:1]

Dtr[1,]




# ----------
# validation data set: X

X <- data[3001:6000, 2]


D <- embed(X, w)




# ------------------------------------------------------------------------------
# Compute distances between series and extract k nearest neighbour distance --> anomaly
# ------------------------------------------------------------------------------

# Fast Nearest Neighbor Search Algorithms and Applications
library(FNN)



# number of neighbours
nk <- 1


d <- knnx.dist(Dtr, D, k = nk)


dim(d)



# anomaly score = average distance to k nearest neighbour
a <- rowMeans(d)




# ----------

graphics.off()

par(mfrow = c(2,1))

rng <- c(1150, 1300)

plot(X, type = "l", main = "original validation series", ylim = c(4, 6))
abline(v = rng, lty = 2, col = "red")

plot(a, type = "l", lty = 1, col = "blue", main = "anomaly score")
abline(v = rng, lty = 2, col = "red")






#######################################################################################
# ------------------------------------------------------------------------------
# How about V3 ?
# ------------------------------------------------------------------------------


# window length: try 200, 100 ,10
w <- 100
w <- 10
# w <- 200



# ----------
# training data set: Xtr

Xtr <- data[1:3000, 3]


# embedding time series
Dtr <- embed(Xtr, w)


dim(Dtr)



# both are same
Xtr[100:1]

Dtr[1,]




# ----------
# validation data set: X

X <- data[3001:6000, 3]


D <- embed(X, w)




# ----------
# number of neighbours
nk <- 1


d <- knnx.dist(Dtr, D, k = nk)


dim(d)



# anomaly score = average distance to k nearest neighbour
a <- rowMeans(d)




# ----------

graphics.off()

par(mfrow = c(2,1))

rng <- c(1250, 1340)

plot(X, type = "l", main = "original validation series", ylim = c(0,10))
abline(v = rng, lty = 2, col = "red")

plot(a, type = "l", lty = 1, col = "blue", main = "anomaly score")
abline(v = rng, lty = 2, col = "red")


