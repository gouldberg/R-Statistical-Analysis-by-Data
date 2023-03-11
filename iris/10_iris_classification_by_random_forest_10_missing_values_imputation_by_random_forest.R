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
# Missing Values imputation by randomForest
#   - impute missing values in predictor data using proximity from randomForest
#   - The algorithm starts by imputingNAs usingna.roughfix. Then randomForest is called with thecompleted data. 
#     The proximity matrix from the randomForest is used to update the imputation of the NAs.
#   - For continuous predictors, the imputed value is the weighted average of the non-missing obervations,
#     where the weights are the proximities. For categorical predictors, the imputed value is the category with the largest average proximity.
#     This process is iterated iter times.
# ------------------------------------------------------------------------------

iris.na <- iris

set.seed(11)

for(i in 1:4) iris.na[sample(150, sample(20)), i] <- NA

head(iris.na)



# ----------
# na.roughfix:  Impute Missing Values by median/mode
# For numeric variables, NAs are replaced with column medians.
# For factor variables, NAs are replaced with the most frequent levels (breaking ties at random).
# If object contains no NAs, it is returned unaltered.
# This is used as a starting point for imputing missing values by random forest
na.roughfix(iris.na)



# ----------
iris.imputed <- rfImpute(Species ~., iris.na)

head(iris.imputed)



# ----------
iris.rf2 <- randomForest(Species ~., iris.imputed)
iris.rf3 <- randomForest(Species ~., iris.na, na.action = na.roughfix)

print(iris.rf2)
print(iris.rf3)

plot(iris.rf2)



