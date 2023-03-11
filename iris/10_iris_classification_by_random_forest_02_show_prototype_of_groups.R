setwd("//media//kswada//MyFiles//R//iris")

packages <- c("dplyr", "randomForest")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  iris
# ------------------------------------------------------------------------------
data("iris")


str(iris)

car::some(iris)



# ------------------------------------------------------------------------------
# Representative cases of a group of data points
#   - They are very similar to medoids.
#     Only computes one prototype per class. For each case in x, then Nbr nearest neighorsare found.
#     Then, for each class, the case that has most neighbors of that class is identified. The prototype for that class is then the medoid of
#     these neighbors (coordinate-wise medians for numerical variables and modes for categorical variables).
# ------------------------------------------------------------------------------

iris.rf <- randomForest(iris[,-5], iris[,5], prox=TRUE)



( iris.p <- classCenter(iris[,-5], iris[,5], iris.rf$prox) )


plot(iris[,3], iris[,4], pch=21, xlab=names(iris)[3], ylab=names(iris)[4],
     bg=c("red", "blue", "green")[as.numeric(factor(iris$Species))],
     main="Iris Data with Prototypes")

points(iris.p[,3], iris.p[,4], pch=21, cex=2, bg=c("red", "blue", "green"))
