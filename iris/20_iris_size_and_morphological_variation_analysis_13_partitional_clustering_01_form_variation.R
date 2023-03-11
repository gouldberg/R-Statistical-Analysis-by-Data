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
# Check ideal number of clusters by k-means elbow criterion
# ------------------------------------------------------------------------------

totv <- sum(diag(var(iris[,1:4]))) * 149

SSratio <- numeric(10)

for(i in 2:10){
  mod <- kmeans(iris[,1:4], i)
  SSratio[i] <- (totv - sum(mod$withinss)) / totv
}

plot(1:10, SSratio, type = "l", xlab = "number of clusters", ylab = "% of explained variance")

points(1:10, c(0, SSratio[2:10]), pch = 3)



# -->
# 2 or 3 clusters is the optimal number of clusters



# ------------------------------------------------------------------------------
# Partitional clustering:  form variation
# ------------------------------------------------------------------------------

library(cluster)

pam(iris[,1:4], 3, stand = F)$clustering


palette(c("black", "grey50"))
par(mar = c(5,4,1,1), mfrow=c(1,2))
plot(pam(iris[,1:4], 3), main = "", col.p = "black")



# -->
# The three groups are represented by different symbols and ellipses are drawn around groups
# Principal component on the correlation matrix with the group as identified by the algorithm.


