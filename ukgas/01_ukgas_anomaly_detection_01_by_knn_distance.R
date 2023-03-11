
packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  UKgas
#    - quarterly gas consumption in UK
# ------------------------------------------------------------------------------


data(UKgas)


str(UKgas)


UKgas




# ------------------------------------------------------------------------------
# data exploration:  time series plot
# ------------------------------------------------------------------------------


ts.plot(UKgas, type = "o")



# ----------
# conver to log
ts.plot(log(UKgas), type = "o")
abline(v = 1970, col = "blue", lty = 2)



# -->
# some structural changes at around 1970



# ----------
# also take 1st difference
ts.plot(diff(log(UKgas)), type = "o")
abline(v = 1970, col = "blue", lty = 2)




# ----------
dat2 <- diff(log(UKgas))




# ------------------------------------------------------------------------------
# Vectorize time series by slide window
# ------------------------------------------------------------------------------


# window length
w <- 3



# ----------
# training data set: Xtr

Xtr <- dat2[1:20]


# embedding time series
Dtr <- embed(Xtr, w)


dim(Dtr)



# both are same
Xtr[w:1]

Dtr[1,]


dim(Dtr)




# ----------
# validation data set: X

X <- dat2[21:length(dat2)]


D <- embed(X, w)


dim(D)




# ------------------------------------------------------------------------------
# Compute distances between series and extract k nearest neighbour distance --> anomaly
# ------------------------------------------------------------------------------

# Fast Nearest Neighbor Search Algorithms and Applications
library(FNN)



# number of neighbours
nk <- 1



# compute distance between time series --> compute distance --> select nk nearest neibour distance
d <- knnx.dist(Dtr, D, k = nk)


dim(d)




# anomaly score = average distance to k nearest neighbour
( a <- rowMeans(d) )




# ----------

graphics.off()

par(mfrow = c(2,1))

plot(X, type = "l", main = "original validation series")

plot(c(rep(NA, w - 1), a), type = "l", lty = 1, col = "blue", main = "anomaly score")



  


