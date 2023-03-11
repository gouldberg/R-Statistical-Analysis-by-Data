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
# Estimating MAP estimate of beta based on Bayesian way
#   - The Bayesian interpretaion of the smoothing penalty gives the model the structure of a linear mixed model
#     and in consequence the MAP estimate of beta:
#     beta | y ~ N(beta-hat, (t(X) %*% X + lambda * S)^-1 * sigma^2)
#
#   - Also having given the model this extra structure opens up the possibility of estimating sigma^2 and lambda using marginal likelihood maximization or REML
# ------------------------------------------------------------------------------

# X in original parameterization
X0 <- tf.X(size, sj)


D <- rbind(0, 0, diff(diag(20), difference = 2))
           

# augmented D
diag(D) <- 1


# re-parameterized X
X <- t(backsolve(t(D), t(X0)))


# mixed model matrices
Z <- X[,-c(1,2)]
X <- X[,1:2]


# profile likelihood
llm <- function(theta, X, Z, y){
  # untransform parameters ...
  sigma.b <- exp(theta[1])
  sigma <- exp(theta[2])
  
  # extract dimensions ...
  n <- length(y);  pr <- ncol(Z);  pf <- ncol(X)
  
  # obtain \hat \beta \hat b...
  X1 <- cbind(X, Z)
  ipsi <- c(rep(0, pf), rep(1/sigma.b^2, pr))
  b1 <- solve(crossprod(X1) / sigma^2 + diag(ipsi), t(X1) %*% y / sigma^2)
  
  # compute
  ldet <- sum(log(diag(chol(crossprod(Z) / sigma^2 + diag(ipsi[-(1:pf)])))))
  
  # compute log profile likelihood
  l <- (-sum((y - X1 %*% b1)^2) / sigma^2 - sum(b1^2 * ipsi) - n * log(sigma^2) - pr * log(sigma.b^2) - 2 * ldet - n * log(2 * pi)) / 2
  attr(l, "b") <- as.numeric(b1)
  return(-l)
}


# estimate smoothing and variance parameters
m <- optim(c(0, 0), llm, method = "BFGS", X = X, Z = Z, y = wear)

m


# extract coefficients
b <- attr(llm(m$par, X, Z, wear), "b")

b


# ----------
graphics.off()
plot(size, wear)
# re-parameterized pred. mat.
Xp1 <- t(backsolve(t(D), t(Xp)))
lines(s, Xp1 %*% as.numeric(b), col = "grey", lwd = 2)



# ------------------------------------------------------------------------------
# The same re-parametrization and estimation using REML via lme()
# ------------------------------------------------------------------------------

library(nlme)


# In lme terms all the data belong to a single group, so to use lme we must create a dummy grouping variable enforcing this.
# A covariance matrix proportional to the identiy matrix is then specified via the pdIdent function.

g <- factor(rep(1, nrow(X)))
m <- lme(wear ~ X - 1, random = list(g = pdIdent(~ Z-1)))


lines(s, Xp1 %*% as.numeric(coef(m)))


# -->
# Notice how the REML based estimate (black) is more variable than the ML based estiamte (grey).



