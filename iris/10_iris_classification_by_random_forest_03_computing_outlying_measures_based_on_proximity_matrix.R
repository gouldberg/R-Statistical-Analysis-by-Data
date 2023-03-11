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
# Compute outlying measures
#   - A numeric vector containing the outlying measures.
#   - The outlying measure of a case is computed as n / sum(squared proximity), normalized by subtracting the median and divided by the MAD,
#     within each class
# ------------------------------------------------------------------------------


iris.rf <- randomForest(iris[,-5], iris[,5], proximity=TRUE)

plot(outlier(iris.rf), type="h", col=c("red", "green", "blue")[as.numeric(iris$Species)])


