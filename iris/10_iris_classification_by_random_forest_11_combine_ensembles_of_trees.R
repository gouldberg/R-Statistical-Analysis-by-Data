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
# Combine ensembles of trees
# ------------------------------------------------------------------------------

rf1 <- randomForest(Species ~ ., iris, ntree=50, nodesize = 3, norm.votes=FALSE)

rf2 <- randomForest(Species ~ ., iris, ntree=50, nodesize = 4, norm.votes=FALSE)

rf3 <- randomForest(Species ~ ., iris, ntree=50, nodesize = 5, norm.votes=FALSE)

rf.all <- combine(rf1, rf2, rf3)

print(rf.all)


# ----------
hist(treesize(rf.all))



# ----------
varImpPlot(rf.all)



# ----------
getTree(rf.all, k = 1, labelVar = TRUE)



# ------------------------------------------------------------------------------
# Add trees to an ensemble
# ------------------------------------------------------------------------------

# Add an additional trees to an existing ensemble of trees
# The confusion, err.rate, mse and rsq components (as well as the corresponding components in the test component, if exist)
# of the combined object will be NULL.

rf.all <- grow(rf.all, 50)



