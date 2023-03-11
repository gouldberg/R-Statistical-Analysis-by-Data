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
# Step-wise selectoin
# ------------------------------------------------------------------------------

stepmod <- step(modlm, direction = "both", k = 2)


stepmod$coefficients





# ------------------------------------------------------------------------------
# Assess the model
# ------------------------------------------------------------------------------

xyplot(stepmod$residuals ~ stepmod$fitted.values, type = c("p", "g", "smooth"))



# -->
# RMSE train vs test  --> overfitting !!!

rmse(stepmod$fit, train$fat)


rmse(predict(stepmod, newdata = test), test$fat)


