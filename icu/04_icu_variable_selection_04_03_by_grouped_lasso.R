setwd("//media//kswada//MyFiles//R//icu")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Death in the ICU
# ------------------------------------------------------------------------------

data("ICU", package = "vcdExtra")

str(ICU)

dim(ICU)


car::some(ICU)



# ----------
# Here we do not apply "rage" and "coma"
# race, coma is represented initially as 3-level factors, but the recoded to binary variables (white, uncons)
# we apply binary variables

ICU2 <- ICU[, -c(4, 20)]

str(ICU2)



# ------------------------------------------------------------------------------
# variable selection by LASSO
#
# The original formation for the LASSO does not handle categorical explanatory variables, so revised version, called the grouped LASSO was developed to correct this.
# The grouped LASSO is available for logistic regression in the SGL package, and for logistic and Poisson regression in the grplasso packages.
# ------------------------------------------------------------------------------

library(glmnet)

var <- setdiff(colnames(ICU2), "died")

yy <- as.matrix((ICU2[,"died"] == "Yes") * 1)

xx <- model.matrix(~ . -1, ICU2[,var])



# ----------
set.seed(27498272)

cv.lasso.fit <- cv.glmnet(y = yy, x = xx, family = "binomial", nfolds = 10)



# ----------
# Check lambda 

plot(cv.lasso.fit)

cv.lasso.fit$lambda.min

cv.lasso.fit$lambda.1se



# ----------
# Coefficient
coef(cv.lasso.fit)



# -->
# Note that only unconsUncons ...



# ----------
# Fitted result
pred.las <- predict(cv.lasso.fit, newx = xx, type = "response")


