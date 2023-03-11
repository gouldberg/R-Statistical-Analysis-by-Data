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
# Partial Least Squares (PLS)
#   - Partial least squares (PLS) is a method for relating a set of input variables and outputs.
#     Principal Components Regression ignores Y in determining the linear combinations, PLS regression explicitly chooses them to predict Y as well as possible.
#
#   - PCR and PLS are particularly attractive methods when there are large numbers of predictors, p, relative to the sample size, n.
#     They can still work even when p > n.
#   - PLS tends to have an advantage over PCR for prediction problems because PLS constructs its linear combination explicitly to predict the response.
#     On the other hand, PCR is better suited for developing insights by forming linear combinations that have interesting interpretations.
# ------------------------------------------------------------------------------

library(pls)

set.seed(123)

plsmod <- plsr(siri ~ ., data = train_fat, ncomp = 15, validation = "CV")

summary(plsmod)



# ----------
par(mfrow=c(1,3))
plot(lmod_step$coef[-1], xlab = "", ylab = "Coefficient", main = "AIC selected least square fit", type = "l")
abline(h = 0, lty = 2)
coefplot(pcrmod, ncomp = 15, xlab = "", main = "PCR ncomp = 15")
coefplot(plsmod, ncomp = 15, xlab = "", main = "PLS")



# ----------
# cross-validated estimates of the RMSE
plsCV <- RMSEP(plsmod, estimate = "CV")

par(mfrow=c(1,1))
plot(plsCV, main = "")



# ----------
# almost same with PCR
rmse(predict(pcrmod, test_fat, ncomp = 7), test_fat$siri)
rmse(predict(plsmod, test_fat, ncomp = 5), test_fat$siri)

