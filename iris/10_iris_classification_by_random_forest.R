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
# Classification by Random Forest
# ------------------------------------------------------------------------------

set.seed(71)

iris.rf <- randomForest(Species ~., data = iris, importance = TRUE, proximity = TRUE)



# ----------
# OOB estimate of error rate and Confusion matrix
print(iris.rf)



# ----------
# Convergence of OOB error
plot(iris.rf)



# ------------------------------------------------------------------------------
# Margins of randomForest classifier
#   - compute or plot the margin of predictions from a randomForest classifier
#   - The margin of a data point is defined as the proportion of votes for the correct class minus maximum proportion of votes for the other classes.
#     Thus under majority votes, positive margin means correct classification, and vice versa.
# ------------------------------------------------------------------------------
( tmp <- margin(iris.rf) )

plot(margin(iris.rf))



# ------------------------------------------------------------------------------
# Variable Importance
# ------------------------------------------------------------------------------

round(importance(iris.rf, scale = TRUE), digits = 3)


varImpPlot(iris.rf)



# -----------
# find out which predictor variables are actually used in random forest
varUsed(iris.rf)
sum(varUsed(iris.rf))

varUsed(iris.rf, by.tree = TRUE)

varUsed(iris.rf, count = FALSE)


# -->
# The third variable (Petal.length) is used 1233 times
# If count = FALSE, a list of integer indices is returned giving the variables used in the trees.



# ------------------------------------------------------------------------------
# Size of trees (number of nodes)
# ------------------------------------------------------------------------------

hist(treesize(iris.rf))


# Grow no more than 4 nodes per tree:
( treesize(randomForest(Species ~ ., data = iris, maxnodes=4, ntree=30)) )



# ------------------------------------------------------------------------------
# Extract a single tree from a forest
# ------------------------------------------------------------------------------

# which tree to extract ?
k <- 1


# For numerical predictors, data with values of the variable less than or equal to the splitting point goto the left daughter node.
# For categorical predictors, the splitting point is represented by an integer, whose binary expansiongives the identities of the categories
# that goes to left or right.
# For example, if a predictor has four categories, and the split point is 13.
# The binary expansion of 13 is  (1,  0,  1,  1)  (because 13 = 1∗20+ 0∗21+ 1∗22+ 1∗23), so
# cases with categories 1, 3, or 4 in this predictor get sentto the left, and the rest to the right.

getTree(iris.rf, k = k, labelVar=TRUE)




# ------------------------------------------------------------------------------
# Multidimensional Scaling plot of proximity matrix from randomForest
# ------------------------------------------------------------------------------
MDSplot(iris.rf, iris$Species)

MDSplot(iris.rf, iris$Species, palette=rep(1, 3), pch=as.numeric(iris$Species))



# ------------------------------------------------------------------------------
# Multidimensional Scaling on 1 - proximity
# ------------------------------------------------------------------------------

iris.mds <- cmdscale(1 - iris.rf$proximity, eig = TRUE)


( output <- cbind(iris[,1:4], iris.mds$points) )


op <- par(pty="s")
pairs(output, cex=0.6, gap=0,
      col=c("red", "green", "blue")[as.numeric(iris$Species)],
      main="Iris Data: Predictors and MDS of Proximity Based on RandomForest")

par(op)


print(iris.mds$GOF)




