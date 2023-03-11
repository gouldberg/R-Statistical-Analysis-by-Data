setwd("//media//kswada//MyFiles//R//heger_priede")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Heger Priede
# ------------------------------------------------------------------------------
BL <- read.table(file = "//media//kswada//MyFiles//references//ZuurBeginnersGuideToGeneralizedAdditiveModelsWithR//HegerPriede.txt", header = TRUE)


str(BL)

names(BL)



# ------------------------------------------------------------------------------
# relationship between Sources and Depth
# ------------------------------------------------------------------------------

# The data were sampled at 14 tstations, but in the beginning our analysis we will ignore any potential station effects.

# scatterplot of depth in metres versus bioluminescence counts per cubic meter (called "Sources" in the data file)

# Scale Depth
BL$DepthOriginal <- BL$Depth
BL$Depth <- BL$Depth/max(BL$Depth)

par(mfrow=c(1,2))
plot(x = BL$DepthOriginal, y = BL$Sources, xlab = "Depth", ylab ="Sources",  cex.lab = 1.5)
plot(x = BL$Depth, y = BL$Sources, xlab = "Scaled depth", ylab ="Sources",  cex.lab = 1.5, cex = 0.7,  pch = 16, col = grey(0.5))



# ------------------------------------------------------------------------------
# 3 smoothers using different values for smoothing parameter lambda
#   - restric the sum of squares of regression parameters for rhs2()
# ------------------------------------------------------------------------------
# quadratic spline regression
M5 <- lm(Sources ~ Depth  + I(Depth^2) +
           rhs2(Depth, 0.159) +
           rhs2(Depth, 0.220) +
           rhs2(Depth, 0.281) +
           rhs2(Depth, 0.344) +
           rhs2(Depth, 0.410) +
           rhs2(Depth, 0.490) +
           rhs2(Depth, 0.567) +
           rhs2(Depth, 0.664) +
           rhs2(Depth, 0.787),
         data = BL)

X <- model.matrix(M5)
coef(M5)
head(X)


# ----------
# K: number of inner knots
K <- 9
D <- diag(rep(1, 3 + K))

# not applying to intercept, Depth and I(Depth^2)
D[1,1] <- D[2,2] <- D[3,3] <-0

D


# ----------
# restrict the sum of squares of regression parameters for rhs2()
MyLambdas <- c(0, 1, 10000)
MyColors <- c("black", "red", "blue")

par(mfrow=c(1,1))
plot(x = BL$Depth, y = BL$Sources, xlab = "Scaled depth", ylab ="Sources",  cex.lab = 1.5, cex = 0.7,  pch = 16,  col = grey(0.5))

i <- 0
for (lambda in MyLambdas){
  # lambda <- 0.01
  # restrict the sum of squares of regression parameters for rhs2()
  Beta.lambda <- solve(t(X) %*% X + lambda^2 * D) %*%t(X) %*% BL$Sources
  print(Beta.lambda)
  MyDepth <- seq(0.1, 1, length =100)
  XPred <- model.matrix(~ 1 + MyDepth + I(MyDepth^2)+
                          rhs2(MyDepth, 0.159) +
                          rhs2(MyDepth, 0.220) +
                          rhs2(MyDepth, 0.281) +
                          rhs2(MyDepth, 0.344) +
                          rhs2(MyDepth, 0.410) +
                          rhs2(MyDepth, 0.490) +
                          rhs2(MyDepth, 0.567) +
                          rhs2(MyDepth, 0.664) +
                          rhs2(MyDepth, 0.787) )
  
  Yhat <- XPred %*% as.vector(Beta.lambda)
  i <- i + 1
  lines(MyDepth, Yhat, lwd = 3, col = MyColors[i])
}


# -->
# The smaller the value of lambda, the more variation in the curve.



# ------------------------------------------------------------------------------
# Cubic smoothing spline
#   - The cubic smoothing spline is included in many text books.
#     Instead of imposing a restriction on the beta parameters for inner knots to control the amount of smoothing,
#     cubic smoothing spline uses a different mechanism to influence the amount of smoothing by using as a penalty:
#     = lambda * sum of the second order derivative of f at all points.
#   - Wood (2006) shows that the function f that minimizes the criterion (= || y - X * beta ||^2 + lambda * integral(f''*dx))
#     is a natural cubic spline.
#     (natural meaning that fiting a linear relationship on the data in the first and last bin to avoid erratic behaviour of the smoother at its edges)
#
#   - R code to estimate the smoother is easy to comprehend but potentially numerically unstable.
#     Software routines such as gam and mgcv use more refined and stable estimation techniques, but the principle is the same.
# ------------------------------------------------------------------------------
# Wood (2006) and Zuur et al. (2009a), along with many others, used a cubic regression spline with a different basis from
# that of the cubic regression spline presented here.
rk <- function(x,z) ((z-0.5)^2-1/12)*((x-0.5)^2-1/12)/4 -((abs(x-z)-0.5)^4 -0.5*(abs(x-z)-0.5)^2 +7/240)/24


spl.X <- function(x,xk){
  q <- length(xk) + 2  # including outer knots (first and last)
  n <- length(x)
  X <- matrix(1, n, q)
  X[,2] <- x
  X[,3:q] <- outer(x, xk, FUN=rk)
  X
}

X  <- spl.X(BL$Depth, QD[2:10])

head(X)



# ----------
# D:  penalty matrix
# Wood 2006: Gives S
spl.S <- function(xk){
  q <- length(xk) + 2
  S <- matrix(0, nrow = q, ncol = q)
  S[3:q, 3:q] <- outer(xk, xk, FUN = rk)
  S
}


# Penalty matrix:
D <- spl.S(QD[2:10])

head(D)



# ----------
# Cross-validation
K      <- 25
OCV    <- vector(length = K)
Lambda <- seq(0, 0.001, length = K)
N      <- nrow(BL)

for (j in 1:K){
  lambda <- Lambda[j]
  EP<- vector (length= N)
  for (i in 1:N){ # Apply the GAM
    Xi    <- X[-i,]
    Betai <- solve(t(Xi) %*% Xi + lambda * D) %*% t(Xi)%*% BL$Sources [-i]
    Yi    <- X[i,] %*% Betai
    EP[i] <- BL$Sources[i] - Yi
  }
  OCV[j] <- sum(EP^2)/N
}


plot(x = Lambda, y = OCV, type = "l", xlab = "lambda", ylab = "OCV")



# ----------
# For a large data set, the calculation above is time consuming.
# Fortunately, a mathematical shortcut exists to calculate Ordinary Cross Validation score in a single GAM
for (j in 1:K){
  lambda <- Lambda[j]
  S      <- X %*% solve(t(X) %*% X + lambda * D) %*%t(X)
  Sii    <- diag(S)
  #Beta  <- solve(t(X) %*% X + lambda * D) %*%t(X) %*% BL$Sources
  Yfit   <- S %*% BL$Sources
  E      <- BL$Sources - Yfit
  OCV[j] <- (1/N) * sum(( E/(1-Sii))^2 )
}



# ----------
# smallest value of lambda
lambda <- 2.083333e-04
Beta   <- solve(t(X) %*% X + lambda * D) %*%t(X) %*% BL$Sources
MD     <- data.frame(Depth = seq(QD[1], QD[11], length =100))
Xp     <- spl.X(MD$Depth, QD[2:10])
fhat   <- Xp %*% Beta

plot(x = BL$Depth, y = BL$Sources, xlab = "Scaled depth", ylab ="Sources",  cex.lab = 1.5, cex = 0.7,  pch = 16, col = grey(0.8))
lines(x = MD$Depth, y =fhat, lwd = 5)




# ------------------------------------------------------------------------------
# Degrees of freedom of a smoother
#  - As in linear regression, the degrees of freedom for a smoother is defined as df = tr(projection matrix)
#    y_hat = projection matrix * y_observed
# ------------------------------------------------------------------------------
# Make a picture that visualises the relationship between lambda and degrees of freedom
# Use a cubic smoothing spline

X        <- spl.X(BL$Depth, QD[2:10])
D        <- spl.S(QD[2:10])   # Penalty matrix
lambda   <- 2.083333e-04
S.lambda <- X %*% solve(t(X) %*% X + lambda * D) %*%t(X)

S.lambda

( df1      <- sum(diag(S.lambda)) )



# -->
# the degrees of freedom of cubic smoothing spline model is 10.1985.
# Note that this includes intercept.


# ----------
# For large values of lambda the degrees of freedom will be small, and for small values of lambda it will be high.
# Also for large lambda values the degrees of freedom is 2, which represents a straight line.
df     <- vector(length = 100)
Lambda <- seq(0,5, length = 100)
for (i in 1:100){
  lambda   <- Lambda[i]
  S.lambda <- X %*% solve(t(X) %*% X + lambda * D) %*%t(X)
  df[i]    <- sum(diag(S.lambda))
}

plot(x = Lambda, y = df, type = "l", xlab = "lambda", ylab = "Effective degrees of freedom")



# ----------
# Alternative definition of the degrees of freedom is the error degrees of freedom
df.Error <- nrow(BL) - sum(diag(2 * S.lambda - S.lambda %*% t(S.lambda)))
df3 <- sum(diag(S.lambda %*% t(S.lambda)))
c(df1, df.Error, df3, nrow(BL))



# ------------------------------------------------------------------------------
# Confidence intervals
# ------------------------------------------------------------------------------

# calculate the smoothed values for 100 depths (yfit)
lambda <- 2.083333e-04
Beta   <- solve(t(X) %*% X + lambda * D) %*%t(X) %*% BL$Sources
Xp     <- spl.X(MD$Depth, QD[2:10])
yfit   <- Xp %*% Beta



# ----------
# estimated variance
S.lambda <- X %*% solve(t(X) %*% X + lambda * D) %*%t(X)
df.Error <- nrow(BL) - sum(diag( 2 * S.lambda - S.lambda %*% t(S.lambda)))
sigma2   <- sum((BL$Sources - X %*% Beta)^2) / (df.Error)
sigma    <- sqrt(sigma2)
sigma



# ---------
# calculate the covariance matrix of the estiamted smoother
Sp.lambda <- Xp %*% solve(t(Xp) %*% Xp + lambda * D) %*%t(Xp)
CovPredY  <- sigma2 * Sp.lambda%*% t(Sp.lambda)



# ----------
# square root of the diagonal elements of covariance matrix --> standard errors for the 100 depth values
SEPredY   <- sqrt(diag(CovPredY))



# ----------
# Estimated cubis smoothing spline and 95% point-wise confidence bands
par(mfrow = c(1,1), mar = c(5,5,3,3))
plot(x = BL$Depth, y = BL$Sources, xlab = "Scaled depth", ylab ="Sources",  cex.lab = 1.5, cex = 0.7,  pch = 16, col = grey(0.5))
lines(x = MD$Depth, y = yfit, lwd = 3)
lines(x = MD$Depth, y = yfit + 2 * SEPredY, lwd = 5)
lines(x = MD$Depth, y = yfit - 2 * SEPredY, lwd = 5)


cbind(yfit - 2 * SEPredY, yfit, yfit + 2 * SEPredY)


