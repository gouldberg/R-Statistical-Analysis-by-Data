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
# Lasso
# ------------------------------------------------------------------------------

library(lars)

set.seed(2018)

cvlmod <- cv.lars(as.matrix(train %>% dplyr::select(-fat)), train$fat, type="lasso", trace="TRUE")



# ----------
min(cvlmod$cv)


cvlmod$index[which.min(cvlmod$cv)]



# ----------

lmod <- lars(as.matrix(train %>% dplyr::select(-fat)), train$fat, type="lasso", trace="TRUE")

plot(lmod)




# ------------------------------------------------------------------------------
# Prediction
# ------------------------------------------------------------------------------

pred <- predict(lmod, s=cvlmod$index[which.min(cvlmod$cv)], type="coef", mode="fraction")

plot(pred$coefficients, type="h", ylab="Coefficients")



sum(pred$coef != 0)



# ----------
pred <- predict(lmod, as.matrix(train %>% dplyr::select(-fat)), s=cvlmod$index[which.min(cvlmod$cv)], mode="fraction")

pred_test <- predict(lmod, as.matrix(test %>% dplyr::select(-fat)), s=cvlmod$index[which.min(cvlmod$cv)], mode="fraction")




# ----------
rmse(pred$fit, train$fat)

rmse(pred_test$fit, test$fat)



