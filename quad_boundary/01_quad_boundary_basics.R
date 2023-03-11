setwd("//media//kswada//MyFiles//R//quad_boundary")

packages <- c("dplyr", "AppliedPredictiveModeling", "caret", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Simulate some two class data with two predictors
# ------------------------------------------------------------------------------
set.seed(975)

training <- quadBoundaryFunc(500)
testing <- quadBoundaryFunc(1000)


str(training)
str(testing)

testing$class2 <- ifelse(testing$class == "Class1", 1, 0)
testing$ID <- 1:nrow(testing)



# ------------------------------------------------------------------------------
# the relationship between 2 variables by class
# ------------------------------------------------------------------------------
# the relationship between 2 variables separately by class (multi panels)
xyplot(X2 ~ X1 | class, data = training)


# ----------
# the relationship between 2 variables in one panel, color by class
ggplot(data = training) + geom_point(mapping = aes(x = X1, y = X2, color = class))

# pch --> changes shape
xyplot(X2 ~ X1, data = training, groups = class, pch = 20)



# ----------
# the relationship between 2 variables in one panel, shape by class
ggplot(data = training) + geom_point(mapping = aes(x = X1, y = X2, shape = class))



# ------------------------------------------------------------------------------
# Different shape (or color) by probabilities
# ------------------------------------------------------------------------------
training$prob2 <- cut(training$prob, breaks = seq(0, 1, by = 0.2))

xyplot(X2 ~ X1 | class, data = training, groups = prob2)



