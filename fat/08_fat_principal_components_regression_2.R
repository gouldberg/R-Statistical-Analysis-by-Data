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
# Principal components analysis
# ------------------------------------------------------------------------------

prfat <- prcomp(train_fat[,c(2:ncol(train_fat))], scale = TRUE)

summary(prfat)



# ----------
# examine the standard deviations of the principal components
round(prfat$sdev, 3)


# elbow plot
plot(prfat$sdev[1:10], type = "l", ylab = "SD of PC", xlab = "PC number")



# ----------
# The linear combinations (or loadings) can be found in the rotation matrix
# we plot these vectors (eigenvectors) against the predictor number
# solid line corresponds to the 1st PC, the dotted line is for the second PC and the dashed line is for the third PC.
matplot(1:(ncol(train_fat)-1), prfat$rot[,1:3], type = "l", col = 1)

matplot(1:(ncol(train_fat)-1), prfat$rot[,1:10], type = "l", col = 1)


# -->
# we see that the rist PC comes from an almost constant combination of the variables.



# ------------------------------------------------------------------------------
# Principal components regression
# ------------------------------------------------------------------------------

library(pls)

pcrmod <- pcr(siri ~ ., data = train_fat, ncomp = 10)



# ----------
# let's use the 1st 4 PCs to predict the response.
rmse(predict(lmod_step), train_fat$siri)
rmse(predict(pcrmod, ncomp = 4), train_fat$siri)
rmse(predict(pcrmod, ncomp = 10), train_fat$siri)


# -----------
# PCR shrinks the estimation of coefficient compared to ful least squares fit.
par(mfrow=c(1,3))
plot(lmod_step$coef[-1], xlab = "", ylab = "Coefficient", type = "l")
abline(h = 0, lty = 2)
coefplot(pcrmod, ncomp = 4, xlab = "", main = "ncomp = 4")
coefplot(pcrmod, ncomp = 10, xlab = "", main = "ncomp = 10")



# ----------
# for test data
rmse(predict(lmod_step, test_fat), test_fat$siri)
rmse(predict(pcrmod, test_fat, ncomp = 4), test_fat$siri)
rmse(predict(pcrmod, test_fat, ncomp = 10), test_fat$siri)



# ----------
# We can figure out how many PCs would give the best result on the test sample. --> 8 PCs
library(ModelMetrics)

pcrmse <- RMSEP(pcrmod, newdata = test_fat)

par(mfrow=c(1,1))
plot(pcrmse, main = "")

which.min(pcrmse$val)
pcrmse$val[which.min(pcrmse$val)]



# ----------
# cross-validated RMSE
set.seed(123)

pcrmod <- pcr(siri ~., data = train_fat, validation = "CV", ncomp = 15)
pcrCV <- RMSEP(pcrmod, estiamte = "CV")
plot(pcrCV, main = "")


pcrCV$val

which.min(pcrCV$val)

ypred <- predict(pcrmod, test_fat, ncomp = which.min(pcrCV$val))
rmse(ypred, test_fat$siri)

