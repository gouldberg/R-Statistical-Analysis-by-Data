# setwd("//media//kswada//MyFiles//R//melanoma")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\meatspec")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  meatspec
# ------------------------------------------------------------------------------

data(meatspec, package="faraway")


str(meatspec)


dim(meatspec)




# ------------------------------------------------------------------------------
# Train and Test split
# ------------------------------------------------------------------------------

train_row <- 1:172

test_row <- 173:215


train <- meatspec[train_row,]

test <- meatspec[test_row,]




# ------------------------------------------------------------------------------
# FUNCTION to calculate RMSE
# ------------------------------------------------------------------------------

rmse <- function(x,y) sqrt(mean((y-x)^2))




# ------------------------------------------------------------------------------
# Ridge Regression
# ------------------------------------------------------------------------------

rgmod <- lm.ridge(fat ~ ., data = train, lambda = seq(0, 5e-8, len=21))



# ----------
# which lambda minimize Generalized Crossvalidation

which.min(rgmod$GCV)



# ----------
plot(rgmod$GCV ~ rgmod$lambda)

abline(v=names(which.min(rgmod$GCV)))



# ----------
matplot(rgmod$lambda, coef(rgmod), type="l", xlab=expression(lambda), ylab=expression(hat(beta)), col=1)

abline(v=names(which.min(rgmod$GCV)))




# ------------------------------------------------------------------------------
# Predict test data
# ------------------------------------------------------------------------------

# ridge regression both centers and scales the predictors, so we need to do the same in computing fit.
# Furthermore, we need to add back in the mean of the response because of the centering.

ypred <- cbind(1, as.matrix(train[,-101])) %*% coef(rgmod)[unname(which.min(rgmod$GCV)),]


ypred_test <- cbind(1, as.matrix(test[,-101])) %*% coef(rgmod)[unname(which.min(rgmod$GCV)),]




# ----------
rmse(ypred, train$fat)


rmse(ypred_test, test$fat)




# ------------------------------------------------------------------------------
# Remove outliers
# ------------------------------------------------------------------------------

# one observation is very bad

i <- 13

c(ypred_test[i], test$fat[i])



# ----------
# so if remove this observation

rmse(ypred_test[-i], test$fat[-i])


