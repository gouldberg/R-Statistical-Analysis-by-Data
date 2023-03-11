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
# Partial dependence plot
#   - gives a graphical depiction of the marginal effect of a variable on the class probability (classification) or response (regression).
# ------------------------------------------------------------------------------

iris.rf <- randomForest(Species~., iris)

partialPlot(x = iris.rf, pred.data = iris, x.var = Petal.Width, which.class = "versicolor")

