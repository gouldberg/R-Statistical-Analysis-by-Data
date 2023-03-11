# setwd("//media//kswada//MyFiles//R//orange")
setwd("C:\\Users\\kouse\\Desktop\\R\\audiometric")

packages <- c("dplyr", "MPsychoR", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  ballistic Missile Example
# ------------------------------------------------------------------------------

ballistic <- read.table("ballistic.txt", header=TRUE, sep="\t")

str(ballistic)

dim(ballistic)

ballistic


# ------------------------------------------------------------------------------
# Principal Component Analysis by computation
# ------------------------------------------------------------------------------

# obj <- ballistic[,1:4]

# obj_m <- apply(obj, 2, mean)
# obj_c <- sweep(obj, 2, STATS = obj_m, FUN="-")
# obj_s <- scale(obj)

# p <- ncol(obj)
# n <- nrow(obj)

p <- 4
n <- 40


# sample covariance matrix
# S <- cov(obj)

S <- as.matrix(ballistic)

( evd <- eigen(S) )


# ----------
plot(evd$values, type="b", xlab="index", ylab="eigenvalue")


# ----------
U <- evd$vectors

L <- t(U) %*% S %*% U

l <- diag(L)


# characteristic root (or eigenvalues)
l
evd$values
plot(evd$values, type="b", xlab="index", ylab="eigenvalue")


# ----------
# variability is preserved measured by S, L, and l
det(S)
det(L)
prod(l)

psych::tr(S)
psych::tr(L)


# ----------
# ratio of eigenvalues to total
cumsum(l) / sum(l)


# ----------
# z-scores
# principal axis transformation:  principal components
# z <- t(U) %*% t(obj_c)
# z


# ----------
# inversion of PC model
# t(obj_m + U %*% z)
# obj


# ----------
# scaling of vectors
V <- U %*% diag(sqrt(l))
W <- U %*% diag(1/sqrt(l))

V %*% t(V)
S

W %*% t(W)
solve(S)


# ----------
# y-scores
# y <- t(W) %*% t(obj_c)

# y has unit variance
# psych::describe(t(y))

# y = z / sqrt(l)
# t(y)
# sweep(t(z), 2, STATS=sqrt(l), FUN="/")


# ----------
# Overall Measure of Variability: T2
# ( T2 <-  t(y) %*% y )

# as.matrix(obj_c) %*% solve(S) %*% t(as.matrix(obj_c))


# ----------
# T2 threshold value

df1 <- p
df2 <- n - p
fval <- qf(p=0.95, df1=df1, df2=df2, lower.tail = TRUE, log.p = FALSE)

( thresh_val <- df1 / df2 * (n-1) * fval )



# ------------------------------------------------------------------------------
# test for equality of roots
# ------------------------------------------------------------------------------




# ------------------------------------------------------------------------------
# Check anomaly by y_score
# ------------------------------------------------------------------------------

# test 2 components model
k <- 2

# if the sample data is
samp_data <- c(15, 10, 20, -5)
samp_mean <- c(0, 0, 0, 0)

( y_score <- t(W[,1:k]) %*% as.matrix(samp_data - samp_mean) )

( qval <- qt(p=1-0.05/2, df=n-1, lower.tail = TRUE, log.p = FALSE) )


# -->
# both of y_score < qval, so neither of these are significant.


# ------------------------------------------------------------------------------
# Check anomaly by T2
# ------------------------------------------------------------------------------

# test 2 components model
k <- 2

df1 <- k
df2 <- n - k
fval <- qf(p=0.95, df1=df1, df2=df2, lower.tail = TRUE, log.p = FALSE)
( thresh_val <- df1 / df2 * (n-1) * fval )


( T2_score <-  t(y_score) %*% y_score )


# -->
# which is also not significant compared with thresh_val


# ------------------------------------------------------------------------------
# Check anomaly by Q-stats
# ------------------------------------------------------------------------------

# test 2-components model
k <- 2

theta1 <- sum(l[(k+1):p])

theta2 <- sum(l[(k+1):p]^2)

theta3 <- sum(l[(k+1):p]^3)

( h0 <- 1 - 2 * theta1 * theta3 / (3 * theta2^2) )

qval <- qnorm(p=0.95, mean=0, sd=1, lower.tail=TRUE)

qstats_thresh <- theta1 * (((qval * sqrt(2 * theta2 * (h0^2)) / theta1) + ((theta2 * h0 * (h0 - 1)) / (theta1^2)) + 1)^(1/h0))


# values of Q higher than this (140.42) are an indication that a data vector cannot be adequately represented by a 2-component model


# ----------
# if the reconstruct principal components (here no original data, but use y)

# recon <- samp_mean + V[,1:k] %*% y[,1:k]
recon <- c(16.9, 14.3, 7.5, -0.1)

resid <- samp_data - recon

( qstats <- t(resid) %*% resid )


# --> 
# larger than qstats_thresh, which is significant.

