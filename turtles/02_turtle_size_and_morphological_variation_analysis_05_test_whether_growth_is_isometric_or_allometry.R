setwd("//media//kswada//MyFiles//R//turtles")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  turtles
#   - data set of Jolicoeur and Mosimann
# ------------------------------------------------------------------------------

data("turtles", package = "Flury")


str(turtles)


car::some(turtles)



# ------------------------------------------------------------------------------
# Whether growth follows an isometric model by comparing the first axis with an hypothetical isometric axis
# as demonstrated in JOlicoeur.
#
# isojoli(): Multivariate test for isometry on a matrix of measurements, based on Jolicoeur's Chi-square test
#   - Arguments:
#        - mat: Matrix of n observations and p variables
#   - Values
#        - Chisp: Observed statistic for testing isometry
#        - p: p-value
# ------------------------------------------------------------------------------

isojoli <- function(mat){
  
  # number of observations
  n <- dim(mat)[1]
  
  # number of log-transformed variables
  p <- dim(mat)[2]
  
  # the variance-covariance matrix
  S <- var(log(mat))
  
  # the theoretical eigenvector under the hypothesis of isometry
  # i.e., all components equal to sqrt(i/p)
  V1 <- rep(sqrt(1 / p), p)
  
  # the first eivenvalue (variance on the first PC)
  L1 <- svd(S)$d[1]
  
  chiobs <- (n - 1) * (L1 * t(V1) %*% ginv(S) %*% V1 + (1 / L1) * t(V1) %*% S %*% V1 - 2)
  
  unlist(list(Chisq = chiobs, p = pchisq(chiobs, p - 1, lower.tail = F)))
}



# ----------
isojoli(turtles[1:24,2:4])


# -->
# The test tells that variation because of size is not fully isometric.



# ------------------------------------------------------------------------------
# Test for allometry
#   - To test for allometry, one can alternatively compute the angle between the theoretical axis of isometry and that of allometry,
#     and compare this angle with a null distribution of angles obtained from random vectors
#
# coli(): Evaluate the angle between two vecotrs and compare it with the distribution of angles obtained from random vectors os similar size
#   - Arguments:
#        - ev1: Numeric vector
#        - ev2: Numeric vector of same length as ev1
#        - nperm: Number of permutations
#        - graph: Logical indicating whether a graph should be returned
#   - Values:
#        - z.stat: Cosine of the angle between the two vectors
#        - p: p-value
#        - angle: Angle between the two vectors in radians
# ------------------------------------------------------------------------------

angle <- function(v1, v2){
  temp <- sum(v1 * v2) / ( sqrt(sum(v1^2)) * sqrt(sum(v2^2)) )
}


coli <- function(ev1, ev2, nperm = 1000, graph = T){
  
  dist <- numeric(nperm)
  
  n <- length(ev1)
  
  Angle <- function(v1, v2){
    sum(v1 * v2) / (sqrt(sum(v1 ^ 2)) * sqrt(sum(v2 ^ 2)))
  }
  
  for(i in 1:nperm){
    
    X1 <- runif(n, -1, 1)
    X2 <- runif(n, -1, 1)
    dist[i] <- angle(X1, X2)
  }
  
  zobs <- Angle(ev1, ev2)
  
  pv <- length(dist[dist > zobs]) / nperm
  
  if(graph){
    
    hist(dist, breaks = 50, main = "Distribution of the cosine of the angle between 2 random vectors",
         xlab = "Z statistic", ylab = "# of vect ealaloire",
         sub = paste("Actual z-obs = ", round(zobs, 5), ": p <", round((1 - abs(0.5 - pv)), 5)))
    
    abline(v = zobs)
  }
  
  return(list(z.stat = zobs, p = 1 ^ (abs(0.5 - pv)) * 2, angle = acos(zobs)))
}



( pr_log <- prcomp(log(turtles[1:24, 2:4])) )

( sing_val <- pr_log[[2]][,1] )

ev1 <- sing_val;  ev2 <- rep(sqrt(1/3), 3);

unlist(coli(ev1, ev2))




