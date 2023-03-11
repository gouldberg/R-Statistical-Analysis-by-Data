setwd("//media//kswada//MyFiles//R//pyrimidines")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  pyrimidines
# ------------------------------------------------------------------------------

data("pyrimidines", package = "faraway")

str(pyrimidines)

car::some(pyrimidines)

# pyrimidines <- pyrimidines[-c(6,72),]



# ------------------------------------------------------------------------------
# Lasso regression
# ------------------------------------------------------------------------------

X <- pyrimidines %>% dplyr::select(-activity) %>% as.matrix()

Y <- pyrimidines$activity


library(glmnet)



# ----------
# Gaussian
# alpha = 1 indicates Lasso regression  (alpha = 0: ridge regression)
lasso.model.cv <- cv.glmnet(x = X, y = Y, family = "gaussian", alpha = 1, nfold = 5, type.measure = "deviance")
plot(lasso.model.cv)
lasso.model.cv$lambda.min



lasso.model <- glmnet(x = X, y = Y, family = "gaussian", lambda = lasso.model.cv$lambda.min, alpha = 1)

lasso.model$beta





