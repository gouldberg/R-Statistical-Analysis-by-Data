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
# Linear Discriminant Analysis
#   - The linear discriminant coefficients are defined from the non-null eigenvectors of the between-group variance-covariance
#     "scaled" by the within-group variance-covariance
#   - lda() function returns:
#        - the group mean
#        - the probability for each observation to belong to a given group
#        - the coefficients of the linear discriminant
#        - the proportion of the trace explained by the discriminant functions (here two, since we have three groups)
# ------------------------------------------------------------------------------

library(MASS)


miris <- as.matrix(iris[,1:4])

lda(miris, as.factor(iris[,5]))



# -->
# The first discriminant function explaines most of the between-group variation
# It opposes flowers with small petals and large seplas with flowers with large petals and small petals



# ------------------------------------------------------------------------------
# Linear Discriminant Analysis: form and shape variables respectively
# ------------------------------------------------------------------------------

size <- apply(miris, 1, prod) ^ (1 / (dim(miris)[2]))

shapeiris <- miris / size

( formlda <- lda(miris, as.factor(iris[,5])) )

( shapelda <- lda(shapeiris, as.factor(iris[,5])) )



# ----------
proj1 <- miris %*% formlda$scaling

proj2 <- shapeiris %*% shapelda$scaling

layout(matrix(c(1,2), 1, 2))

plot(proj1, pch = (1:3)[as.factor(iris[,5])], asp = 1, cex = 0.6, xlab = "FD1", ylab = "FD2", main = "Form FDA")

plot(proj2, pch = (1:3)[as.factor(iris[,5])], asp = 1, cex = 0.6, xlab = "FD1", ylab = "FD2", main = "Shape FDA")



# ------------------------------------------------------------------------------
# Compute the distances between mean groups (Mahalanobis distances) in the shape defined by discriminant functions
#
#   - One can understand the Mahalanobbis distance as the distance of the point from the center of the group divided by the width of the 
#     ellipsoid in the direction of the tested point.
#     This distance measures the distance between a point and the group mean, taking into account the within-group covariance-variance matrix
# ------------------------------------------------------------------------------
meangroup <- formlda$means

meanproj <- meangroup %*% formlda$scaling

dist(meanproj)



# ----------
# Compute the Mahalanobis distance between the first two groups "by hand"
W <- var(miris[1:50,]) * 49 + var(miris[51:100,]) * 49 + var(miris[101:150,]) * 49

VCVw <- W / (150 - 3)

sqrt(diff(meangroup[1:2,]) %*% solve(VCVw) %*% t(diff(meangroup[1:2,])))



# ----------
# Or, for simplicity, use the predict function
dist(predict(formlda, meangroup)$x)




# ------------------------------------------------------------------------------
# Model disgnostics:  Posterior probabilities of observations
#   - If allocation of observations is always exact, one can infer that differences between groups are significant.
# ------------------------------------------------------------------------------

model <- lda(iris[1:148, 1:4], as.factor(iris[1:148, 5]))


predict(model, iris[149:150, 1:4])


