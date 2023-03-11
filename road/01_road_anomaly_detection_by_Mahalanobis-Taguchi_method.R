rm(list = ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\road")



# ------------------------------------------------------------------------------
# data:  road
#    - data of 26 states in USA
#      -  deaths:  deaths in traffic accicident
#      -  drivers
#      -  popden:  population density
#      -  rural:   road length in rural area
#      -  temp:    average of daily max temperature in January
#      -  fuel:    fuel consumption
# ------------------------------------------------------------------------------


library(MASS)


str(road)


car::some(road)





# ------------------------------------------------------------------------------
# data exploration
# ------------------------------------------------------------------------------


library(car)

formula <- ~ drivers + popden + rural + temp + fuel + deaths


scatterplotMatrix(formula, data = road,
                  smooth = FALSE,
                  id = list(n = 3), ellipse = TRUE, col = gray(0.3), pch = 20)




# -->
# California is outliers
# may due to "fuel"





# ------------------------------------------------------------------------------
# Anomaly detection for the data assumed to follow multivariate normal distribution
#
#   - Mahalanobis-Taguchi Method  (MT method or MT system (MTS))
# ------------------------------------------------------------------------------


X <- road


X$deaths <- X$deaths / X$drivers

X$fuel <- X$fuel / X$drivers


# remove drivers and log
X <- as.matrix(log(X[,-2] + 1))



library(car)

formula <- ~ popden + rural + temp + fuel + deaths


scatterplotMatrix(formula, data = X,
                  smooth = FALSE,
                  id = list(n = 3), ellipse = TRUE, col = gray(0.3), pch = 20)





# ----------
mx <- colMeans(X)


# demean 
Xc <- X - matrix(1, nrow(X), 1) %*% mx




# ----------
# compute covariance matrix

( Sx <- t(Xc) %*% Xc / nrow(X) )




# ----------
# anomaly score

( a <- rowSums((Xc %*% solve(Sx)) * Xc) )



# ----------
# average anomaly score by variable

a <- a / ncol(X)


plot(a, xlab = "index", ylab = "anomaly socre", ylim = c(-1, 30) / ncol(X))


# threthold is set as 1
n <- nrow(road)
lines(0:n, rep(1, length(0:n)), col = "red", lty = 2)


rownames(X)[a > 1]



# -->
# 6 states:  Alaska, Clif, Conn, DC, Maine, Minn are detected




# ----------
# Compute SN ratio of each variable for California 

xc_prime <- Xc["Calif", ]

( SN1 <- 10 * log10(xc_prime ^ 2 / diag(Sx)) )



barplot(SN1)



# -->
# California is outlier, mainly due to fuel





