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
# cross-validation for feature selection
# ------------------------------------------------------------------------------

set.seed(647)

myiris <- cbind(iris[1:4], matrix(runif(96 * nrow(iris)), nrow(iris), 96))



# ----------
# rfcv() shows the cross-validated prediction performance of models with sequentially reduced number of predictors
# (ranked by variable importance) via a nested cross-validation procedure.

result <- rfcv(myiris, iris$Species, cv.fold=3)


# vector of number of variables used at each step
result$n.var

# corresponding vector of error rates or MSEs at each step
result$error.cv

# predicted values from the cross-validation
result$predicted


with(result, plot(n.var, error.cv, log="x", type="o", lwd=2))

