setwd("//media//kswada//MyFiles//R//engine")

packages <- c("dplyr", "lattice", "gamair")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  engine
# ------------------------------------------------------------------------------
data(engine)
attach(engine)

str(engine)



# ------------------------------------------------------------------------------
# basics visualization
# ------------------------------------------------------------------------------

plot(size, wear, xlab = "Engine capacity", ylab = "Wear index")



# ------------------------------------------------------------------------------
# tent function basis
# ------------------------------------------------------------------------------

tf <- function(x, xj, j){
  # generate jth tent function from set defined by knots xj
  dj <- xj * 0
  dj[j] <- 1
  approx(xj, dj, x)$y
}



tf.X <- function(x, xj){
  # tent function basis matrix given data x and knot sequence xj
  n <- length(x)
  nk <- length(xj)
  X <- matrix(NA, n, nk)
  for(j in 1:nk) X[,j] <- tf(x, xj, j)
  X
}



# ------------------------------------------------------------------------------
# penalized piecewise linear smoother
# ------------------------------------------------------------------------------

# sp: smoothing parameter, controls the trade-off between smoothess of the estimated f and fidelity to the data.
# sp = 0 results in an un-penalized piecewise linear regression estimate.

# diff(diag(length(xj)), differences = 2) applies second order differencing to each column of the rank k identity matrix.

# nrow(D) = nrow(X) - 2 since 2nd differences

prs.fit <- function(y, x, xj, sp){
  X <- tf.X(x, xj)  # model matrix
  D <- diff(diag(length(xj)), differences = 2)  # sqrt penalty
  X <- rbind(X, sqrt(sp) * D)  # augmented model matrix
  y <- c(y, rep(0, nrow(D)))  # augmented data
  lm(y ~ X - 1)  # penalized least square fit
}


# ----------
# generate knots
k <- 20
sj <- seq(min(size), max(size), length = k)



# penalized fit
sp <- 2
b <- prs.fit(wear, size, sj, sp)
summary(b)


plot(size, wear)
Xp <- tf.X(s, sj)
lines(s, Xp %*% coef(b))



# ------------------------------------------------------------------------------
# Choosing the smoothing parameter, sp, by generalized cross validation score (GCV)
# ------------------------------------------------------------------------------

tune_n <- 90

rho <- seq(-9, 11, length = tune_n)

n <- length(wear)

V <- rep(NA, tune_n)



# ----------
# Note that the influence() function returns a list of diagnostics including hat, an array of the elements on the leading diagonal of the influence/hat
# matrix of the augmented model.
# The first n of these are the leading diagonal of the influence matrix of the penalized model

for(i in 1:tune_n){
  b <- prs.fit(wear, size, sj, exp(rho[i]))
  tfF <- sum(influence(b)$hat[1:n])
  rss <- sum((wear - fitted(b)[1:n])^2)
  V[i] <- n * rss / (n - tfF)^2  # GCV score
}


V


# plot GCV function against log smoothing parameter
plot(rho, V, type = "l", xlab = expression(log(lamda)), main = "GCV score")



# ----------
( sp <- exp(rho[V == min(V)]) )

b <- prs.fit(wear, size, sj, sp)

plot(size, wear, main = "GCV optimal fit")
lines(s, Xp %*% coef(b))



