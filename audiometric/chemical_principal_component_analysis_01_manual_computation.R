# setwd("//media//kswada//MyFiles//R//orange")
setwd("C:\\Users\\kouse\\Desktop\\R\\audiometric")

packages <- c("dplyr", "MPsychoR", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  checmical
# ------------------------------------------------------------------------------

chemical <- read.table("chemical.txt", header=TRUE, sep="\t")

str(chemical)

dim(chemical)

chemical


# ------------------------------------------------------------------------------
# Principal Component Analysis by computation
# ------------------------------------------------------------------------------

obj <- chemical[,2:3]

obj_m <- apply(obj, 2, mean)
obj_c <- sweep(obj, 2, STATS = obj_m, FUN="-")
obj_s <- scale(obj)

p <- ncol(obj)
n <- nrow(obj)


# sample covariance matrix
S <- cov(obj)

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
z <- t(U) %*% t(obj_c)
z


# ----------
# inversion of PC model
t(obj_m + U %*% z)
obj


# ----------
# scaling of vectors
V <- U %*% diag(sqrt(l))
W <- U %*% diag(1/sqrt(l))

V %*% t(V)
S

W %*% t(W)
solve(S)


# ----------
# residual from 1st pc
( resid1 <- S - V[,1] %*% t(V[,1]) )
  
# residual from 1st and 2nd PCs
( resid2 <- S - V[,1] %*% t(V[,1]) - V[,2] %*% t(V[,2]) )

  
# ----------
# y-scores
y <- t(W) %*% t(obj_c)

# y has unit variance
psych::describe(t(y))

# y = z / sqrt(l)
t(y)
sweep(t(z), 2, STATS=sqrt(l), FUN="/")


# ----------
# Overall Measure of Variability: T2
( T2 <-  t(y) %*% y )

as.matrix(obj_c) %*% solve(S) %*% t(as.matrix(obj_c))


# ----------
# T2 threshold value

df1 <- p
df2 <- n - p
fval <- qf(p=0.95, df1=df1, df2=df2, lower.tail = TRUE, log.p = FALSE)

( thresh_val <- df1 / df2 * (n-1) * fval )


