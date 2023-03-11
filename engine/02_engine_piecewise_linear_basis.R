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



# ----------
# generate knots
k <- 6
sj <- seq(min(size), max(size), length = k)


# get the model matrix and fit model
# Note that rowSums(X) is all 1
( X <- tf.X(size, sj) )

b <- lm(wear ~ X - 1)
summary(b)


# prediction data and matrix 
s <- seq(min(size), max(size), length = 200)
( Xp <- tf.X(s, sj) )



# ----------
graphics.off()
matplot(X, type = "l")

plot(size, wear)
lines(s, Xp %*% coef(b))


