# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
rm(list=ls())
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr", "lattice", "ggplot2", "AER", "car", "lmtest", "stargazer", "broom", "knitr", "tseries", "forecast", "MTS", "vars")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# Simulate Cointegrated Time Series
# ------------------------------------------------------------------------------
n <- 1000


# 4 white noises (stationary time series)
u1 <- rnorm(n, mean = 0, sd = 1)
u2 <- rnorm(n, mean = 0, sd = 1)
u3 <- rnorm(n, mean = 0, sd = 1)
u4 <- rnorm(n, mean = 0, sd = 1)


# 2 independent random walk processes (unit root processes)
e1 <- rnorm(n, mean = 0, sd = 1)
e2 <- rnorm(n, mean = 0, sd = 1)
w0 <- 0
w1 <- c()
w2 <- c()
for(i in 1:n){
  if(i == 1){ w1[i] = w0;  w2[i] = w0; }
  else{ w1[i] <- w1[i-1] + e1[i]; w2[i] <- w2[i-1] + e2[i]; }
}


# Construct 4 time series
x <- w1 + u1
y <- 2 * w1 + u2
s <- w2 + u3
v <- w1 + 2 * w2 + u4


data <- cbind(x, y, s, v)
MTSplot(data)


# ------------------------------------------------------------------------------
# Cointegration Test
#  - Johansen's cointegration test
#
# Estimate ECM-VAR model
# ------------------------------------------------------------------------------
library(urca)


# Johansen's cointegration test
# Choose Error Correction Model:  spec = c("transitory") or spec = c("longrun")
# Choose Test Statistics:  type = c("trace") or type = c("eigen")
# Order p is denoted by K
# Deterministic term c(t):  ecdet = c("none"), "const" or "trend"


# ------------------------------------------
# The relationship between x and y
MTS::VARorderI(data[,c(1,2)])

# eigenvalues are: 0.303,  0.004,  0
# we cannot reject r <= 1, the matrix in the ECM is of rank 1 and there is 1 cointegrating vector.
# 1 cointegrating normalized vector is g1 = (1, -0.503)
summary(ca.jo(data[,c(1,2)], K = 2, ecdet = c("const"), spec = c("transitory"), type = c("eigen")))

# The cointegration series
w1t <- data[,1] - 0.503 * data[,2]
tmp <- cbind(data[,c(1,2)], w1t)
MTSplot(tmp)

# Reject H0: Unit Root in w1t
adfTest(w1t, lags = 2, type = "nc")


# ------------------------------------------
# The relationship between x and s
MTS::VARorderI(data[,c(1,3)])


# eigenvalues are: 0.008,  0.003,  0
# we cannot reject r = 0, no cointegrating vector.
summary(ca.jo(data[,c(1,3)], K = 4, ecdet = c("const"), spec = c("transitory"), type = c("eigen")))

MTSplot(data[,c(1,3)])


# ------------------------------------------
# The relationship between x, y and w
MTS::VARorderI(data[,c(1,2,3)])

# eigenvalues are: 0.249,  0.008,  0.003,  0
# we cannot reject r <= 1, the matrix in the ECM is of rank 1 and there is 1 cointegrating vector.
# 1 cointegrating normalized vector is g1 = (1, -0.502, 0.002)
summary(ca.jo(data[,c(1,2,3)], K = 3, ecdet = c("const"), spec = c("transitory"), type = c("eigen")))

# The cointegration series
w1t <- data[,1] - 0.502 * data[,2] + 0.002 * data[,3]
tmp <- cbind(data[,c(1,2,3)], w1t)
MTSplot(tmp)

# Reject H0: Unit Root in w1t
adfTest(w1t, lags = 3, type = "nc")


# ------------------------------------------
# The relationship between x, y, w and v
MTS::VARorderI(data[,c(1,2,3,4)])

# eigenvalues are: 0.335,  0.299,  0.008,  0,  0
# we cannot reject r <= 2, the matrix in the ECM is of rank 2 and there are 2 cointegrating vectors.
# 2 cointegrating normalized vector is g1 = (1, -1.131, -2.500, 1.249)  and  g2 = (...)  -->  IT IS NOT MATCHED THE ANSWER !!!
summary(ca.jo(data[,c(1,2,3,4)], K = 2, ecdet = c("const"), spec = c("transitory"), type = c("eigen")))

# The cointegration series
w1t <- data[,1] - 0 * data[,2] + 2 * data[,3] - data[,4]
tmp <- cbind(data[,c(1,2,3,4)], w1t)
MTSplot(tmp)

# Reject H0: Unit Root in w1t
adfTest(w1t, lags = 2, type = "nc")


