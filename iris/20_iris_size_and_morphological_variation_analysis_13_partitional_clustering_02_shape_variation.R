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
# Partitional clustering:  shape variation
# ------------------------------------------------------------------------------

miris <- as.matrix(iris[,1:4])

size <- apply(miris, 1, prod) ^ (1 / dim(miris)[2])

shapeiris <- miris / size




# ----------
# Check ideal number of clusters by k-means elbow criterion

totv <- sum(diag(var(shapeiris[,1:4]))) * 149

SSratio <- numeric(10)

for(i in 2:10){
  mod <- kmeans(iris[,1:4], i)
  SSratio[i] <- (totv - sum(mod$withinss)) / totv
}

plot(1:10, SSratio, type = "l", xlab = "number of clusters", ylab = "% of explained variance")

points(1:10, c(0, SSratio[2:10]), pch = 3)


# -->
# 8 clusters ..?



# ----------
pam(shapeiris[,1:4], 8, stand = F)$clustering


palette(c("black", "grey50"))
par(mar = c(5,4,1,1), mfrow=c(1,2))
plot(pam(iris[,1:4], 3), main = "", col.p = "black")
plot(pam(shapeiris[,1:4], 8), main = "", col.p = "black")



