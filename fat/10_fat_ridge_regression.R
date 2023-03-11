setwd("//media//kswada//MyFiles//R//fat")

packages <- c("dplyr", "faraway")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  fat
# ------------------------------------------------------------------------------

data(fat, package="faraway")

str(fat)



# ------------------------------------------------------------------------------
# split training and test data
# ------------------------------------------------------------------------------
var <- c("siri", "age", "weight", "height", "adipos", "free", "neck", "chest", "abdom", "hip", "thigh", "knee", "ankle", "biceps", "forearm", "wrist")


# take test data every tenth observation
test_id <- seq(10, nrow(fat), by = 10)

test_fat <- fat[test_id, var]



# ----------
# training data
train_id <- setdiff(1:nrow(fat), test_id)

train_fat <- fat[train_id, var]


nrow(test_fat)
nrow(train_fat)



# ------------------------------------------------------------------------------
# Ridge Regression
#   - Ridge regression makes the assumption that the regression coefficients (after normalization) should not be very large.
#     Ridge regression is particularly effective when the model matrix is collinear and the usual least squares estimates of beta appear to be unstable.
#   - The use of ridge regression can also be justfied from a Bayesian perspective where a prior distribution on the parameters puts more weight on smaller values.
#   - Ridge regression estimates of coefficients are biased. Ridge regression trade-off of reduction in variance at the price of an increase in bias.
# ------------------------------------------------------------------------------

library(MASS)

rgmod <- lm.ridge(siri ~ ., data = train_fat, lambda = seq(0, 5e-8, len = 21))



# ----------
# The ridge trace plot.
# we can select the value of lambda using generalized crossvalidation (GCV) which is similar to CV but is easier to compute.
matplot(rgmod$lambda, coef(rgmod), type = "l", xlab = expression(lambda), ylab = expression(hat(beta)), col = 1)

which.min(rgmod$GCV)



# ----------
# Ridge regression both centers and scales th epredictors, so we need to do the same in computing the fit.
# Furthermore, we need to add back in the mean of the response because of the centering.

ypred <- cbind(1, as.matrix(train_fat[,2:ncol(train_fat)])) %*% coef(rgmod)[which.min(rgmod$GCV),]

rmse(ypred, train_fat$siri)


ytpred <- cbind(1, as.matrix(test_fat[,2:ncol(train_fat)])) %*% coef(rgmod)[which.min(rgmod$GCV),]

rmse(ytpred, test_fat$siri)

