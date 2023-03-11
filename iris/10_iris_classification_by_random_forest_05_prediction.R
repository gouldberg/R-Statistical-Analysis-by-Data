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
# Prediction
# ------------------------------------------------------------------------------

ind <- sample(2, nrow(iris), replace = TRUE, prob=c(0.8, 0.2))


# -----------
iris.rf <- randomForest(Species ~ ., data=iris[ind == 1,])

iris.pred <- predict(iris.rf, iris[ind == 2,])

iris.pred



table(observed = iris[ind==2, "Species"], predicted = iris.pred)



# ----------
# Get prediction for all trees
predict(iris.rf, iris[ind == 2,], predict.all=TRUE)



# ----------
# Proximities
predict(iris.rf, iris[ind == 2,], proximity=TRUE)



# ----------
# Nodes Matrix
predict(iris.rf, iris[ind == 2,], nodes=TRUE)

str(attr(predict(iris.rf, iris[ind == 2,], nodes=TRUE), "nodes"))




