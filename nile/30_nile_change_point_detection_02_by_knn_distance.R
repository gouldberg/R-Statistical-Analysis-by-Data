rm(list=ls())

packages <- c("dplyr", "dlm", "KFAS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Nile
# ------------------------------------------------------------------------------


data(Nile)


dim(Nile)


str(Nile)




# ------------------------------------------------------------------------------
# data exploration:  time series plot
# ------------------------------------------------------------------------------


ts.plot(Nile)




# ------------------------------------------------------------------------------
# Vectorize time series by slide window
# ------------------------------------------------------------------------------


# window length
w <- 5



# ----------
# training data set: Xtr

Xtr <- Nile[1:30]


# embedding time series
Dtr <- embed(Xtr, w)


dim(Dtr)






# ----------
# validation data set: X

X <- Nile[31:length(Nile)]


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

rng <- c(11)

plot(X, type = "l", main = "original validation series")
abline(v = rng, lty = 2, col = "red")

plot(a, type = "l", lty = 1, col = "blue", main = "anomaly score")
abline(v = rng, lty = 2, col = "red")





