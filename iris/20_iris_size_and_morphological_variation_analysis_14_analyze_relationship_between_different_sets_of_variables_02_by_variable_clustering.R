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
# Analyzing relationships between different sets of variables by variable clustering
#   - One can compare the branching patterns between species in trees for interpreting changes of covariation
# ------------------------------------------------------------------------------

library(Hmisc)

tmp1 <- iris %>% filter(Species == "setosa")
tmp2 <- iris %>% filter(Species == "versicolor")
tmp3 <- iris %>% filter(Species == "virginica")


plot(varclus(as.matrix(tmp1[,1:4]), method = "ave"))
title("setosa")

plot(varclus(as.matrix(tmp2[,1:4]), method = "ave"))
title("versicolor")

plot(varclus(as.matrix(tmp3[,1:4]), method = "ave"))
title("virginica")



# -->
# Measurements are clustered in Iris setosa and Iris versicolor according to structures (sepal or petal)
# while the measurements are clustered according analogous shape measurements (length and width) in Iris virginica


