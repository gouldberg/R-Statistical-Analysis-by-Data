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
# Analyzing relationships between different sets of variables by two-block partial least-squares
#   - The iris dataset actually has two categories of measurements: two on petals and two on sepals.
#     This dataset allows the study of relationships not only between two measurements but betwen two structures: sepal and petal.
#     Here we want to discover whether there is a relationship between sepal measurements and petal measuremnts

#
# pls():  Compute the singular values, singular vectors, and the Rv coefficient from two subsets of variables organized as two matrices.
#   - Arguments:
#        - M1: First variable subset arranged in a matrix of n observations and of p1 variables
#        - M2: Second variable subset arranged in a matrix of n observations and of p2 variables
#   - Values
#        - Rv: Rv coefficient
#        - F1: Singular vectors for the first set
#        - F2: Singular vectors for the second set
#        - D: Singular values
# ------------------------------------------------------------------------------

pls <- function(M1, M2){
  
  p1 <- dim(M1)[2]
  
  p2 <- dim(M2)[2]
  
  n <- dim(M1)[1]
  
  sM12 <- svd(var(cbind(M1, M2))[1:p1, (p1 + 1):(p1 + p2)])
  
  vM12 <- var(cbind(M1, M2))[1:p1, (p1 + 1):(p1 + p2)]
  
  vM21 <- var(cbind(M1, M2))[(p1 + 1):(p1 + p2), 1 : p1]
  
  v11 <- var(M1)
  
  v22 <- var(M2)
  
  D <- sM12$d
  
  F1 <- sM12$u
  
  F2 <- sM12$v
  
  Rv <- sum(diag(vM12 %*% vM21)) / sqrt(sum(diag(v11 %*% v11)) * sum(diag(v22 %*% v22)))
  
  list(Rv = Rv, F1 = F1, F2 = F2, D = D)
  
}



# ----------
# The covariation between petal and sepal forms is incestigated for the 1st species of the data set
tmp <- iris %>% filter(Species == "setosa")

pls1 <- pls(tmp[,1:2], tmp[,3:4])
pls1


# -->
# The covariation between sepal and petal morthologies is rather low (Rv = 0.0760 < 0.1)

# From singular values (D), we notice that most of the covariation for shape or form is concentrated on the 1st dimension of covariation.
# In temrs of morphology, variables are all similarly signed on the first axes, indicating that covariation is primarily explained by size.
# (big petals are found with big sepals)

# The 2nd axis shows a second pattern of covariation with longer than wide petals related with longer than wide sepals.

# We can interpret the 1st axis as a covariation axis because of isometric growth,
# and the second as a major axis of shape covariation.



# ----------
tmp <- iris %>% filter(Species == "versicolor")
# tmp <- iris %>% filter(Species == "virginica")

( pls(tmp[,1:2], tmp[,3:4]) )


# -->
# These covariations are stronger for the second species (Rv = 0.59)
# This shows that petal and sepal morphologies are more coordinated in the second species than in the others.



# ----------
# cancor(tmp[,1:2], tmp[,2:4])


