setwd("//media//kswada//MyFiles//R//iris")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  iris
# ------------------------------------------------------------------------------
data("iris")


str(iris)

car::some(iris)



# ------------------------------------------------------------------------------
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
  
  return(list(z.stat = zobs, p = 1 - (abs(0.5 - pv)) * 2, angle = acos(zobs)))
}



# ------------------------------------------------------------------------------
# Comparing covariation or disimilarity patterns between two groups: test for collinearity
#   - investigate similarity between singular or eigen vectors
#     Singular vectors or eigenvectors of the different covariance matrices represent the main axes of covariation
#     This tests whether the principal axes are collinear.
# ------------------------------------------------------------------------------

tmp1 <- iris %>% filter(Species == "setosa")
tmp2 <- iris %>% filter(Species == "versicolor")
tmp3 <- iris %>% filter(Species == "virginica")



# ----------
res <- matrix(NA, 4, 4)


# we can perform every possible comparison between singular vectors, and store them in a matrix
for(i in 1:4){
  for(j in 1:i){
    res[i,j] <- coli(svd(var(tmp1[,1:4]))$u[,i], svd(var(tmp3[,1:4]))$u[,j], graph = FALSE)$p 
  }
}


res



# -->
# We find that most singular vectors are similar.
