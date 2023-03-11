# ------------------------------------------------------------------------------
# Predict fat by meatspec variable
# 
# Ridge regression:  package MASS
# Lasso:  package lars
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R/")

packages <- c("faraway", "dplyr", "Hmisc", "lattice", "ggplot2", "corrplot", "MASS", "lars", "caret")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# Load data
# ------------------------------------------------------------------------------
data(meatspec, package="faraway")
str(meatspec)
dim(meatspec)


# ------------------------------------------------------------------------------
# Basic analysis
# ------------------------------------------------------------------------------
meatspec %>% head()

describe(meatspec)

# variable clustering
coln <- colnames(meatspec)
char0 <- paste0(coln, collapse="+")
eval(parse(text = paste0("vc <- varclus( ~ ", char0, ", sim='hoeffding', data = meatspec)")))
plot(vc)


# near zero variance variable
nearZeroVar(meatspec)


# find variables with pairwise high correlation (absolute value) --> Many variables are correlated .... difficult for normal regression
corThresh <- 0.9
tooHigh <- findCorrelation(cor(meatspec), corThresh)
names(meatspec)[tooHigh]


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
# Normal Regression
# ------------------------------------------------------------------------------
modlm <- lm(fat ~., data = train)
summary(modlm)

stepmod <- step(modlm, direction = "both", k = 2)
stepmod$coefficients
xyplot(stepmod$residuals ~ stepmod$fitted.values, type = c("p", "g"))


# RMSE train vs test  --> overfitting !!!
rmse(stepmod$fit, train$fat)
rmse(predict(stepmod, newdata = test), test$fat)



# ------------------------------------------------------------------------------
# Ridge Regression
# ------------------------------------------------------------------------------
rgmod <- lm.ridge(fat ~ ., data = train, lambda = seq(0, 5e-8, len=21))

# which lambda minimize Generalized Crossvalidation
which.min(rgmod$GCV)
plot(rgmod$GCV ~ rgmod$lambda)
abline(v=names(which.min(rgmod$GCV)))

matplot(rgmod$lambda, coef(rgmod), type="l", xlab=expression(lambda), ylab=expression(hat(beta)), col=1)
abline(v=names(which.min(rgmod$GCV)))


# ridge regression both centers and scales the predictors, so we need to do the same in computing fit.
# Furthermore, we need to add back in the mean of the response because of the centering.
ypred <- cbind(1, as.matrix(train[,-101])) %*% coef(rgmod)[unname(which.min(rgmod$GCV)),]
ypred_test <- cbind(1, as.matrix(test[,-101])) %*% coef(rgmod)[unname(which.min(rgmod$GCV)),]

rmse(ypred, train$fat)
rmse(ypred_test, test$fat)

# one observation is very bad.
i <- 13
c(ypred_test[i], test$fat[i])

# so if remove this observation
rmse(ypred_test[-i], test$fat[-i])


# ------------------------------------------------------------------------------
# Lasso
# ------------------------------------------------------------------------------
set.seed(2018)
cvlmod <- cv.lars(as.matrix(train %>% dplyr::select(-fat)), train$fat, type="lasso", trace="TRUE")
min(cvlmod$cv)
cvlmod$index[which.min(cvlmod$cv)]


lmod <- lars(as.matrix(train %>% dplyr::select(-fat)), train$fat, type="lasso", trace="TRUE")
plot(lmod)

pred <- predict(lmod, s=cvlmod$index[which.min(cvlmod$cv)], type="coef", mode="fraction")
plot(pred$coefficients, type="h", ylab="Coefficients")
sum(pred$coef != 0)

pred <- predict(lmod, as.matrix(train %>% dplyr::select(-fat)), s=cvlmod$index[which.min(cvlmod$cv)], mode="fraction")
pred_test <- predict(lmod, as.matrix(test %>% dplyr::select(-fat)), s=cvlmod$index[which.min(cvlmod$cv)], mode="fraction")

rmse(pred$fit, train$fat)
rmse(pred_test$fit, test$fat)



