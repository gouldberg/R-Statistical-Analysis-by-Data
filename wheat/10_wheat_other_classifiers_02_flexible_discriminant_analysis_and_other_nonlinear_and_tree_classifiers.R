setwd("//media//kswada//MyFiles//R//wheat")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  wheat
# ------------------------------------------------------------------------------

wheat <- read.csv(file = "//media//kswada//MyFiles//references//AnalysisOfCategoricalDataWithR//Chapter3//wheat.csv")

str(wheat)

car::some(wheat)



# ------------------------------------------------------------------------------
# multinomial logit model and proportional odds model
# ------------------------------------------------------------------------------
# excluding "class" and "moisture"

library(nnet)

# mod.fit <- multinom(formula = type ~ class + density + hardness + size + weight + moisture, data = wheat)
mod.fit <- multinom(formula = type ~ density + hardness + size + weight, data = wheat)



# ----------
# Order properly the type factor
levels(wheat$type)
wheat$type.order <- factor(wheat$type, levels = c("Scab", "Sprout", "Healthy"))
levels(wheat$type.order)

library(MASS)

# The method = "logistic" argument value instructs to use the logit transformation on the cumulative probabilities
# mod.fit.ord <- polr(formula = type.order ~ class + density + hardness + size + weight + moisture, data = wheat, method = "logistic")
mod.fit.ord <- polr(formula = type.order ~ density + hardness + size + weight, data = wheat, method = "logistic")


summary(mod.fit.ord)
# summary(mod.fit.ord2)



# ----------
AIC(mod.fit, mod.fit.ord)



# ------------------------------------------------------------------------------
# Nonlinear Classification Models:  Flexible Discriminant Analysis
#   - Hastie et al. (1994) describe a process where, for C classes, a set of C linear regression models can be fit to binary class indicators and show
#     that the regression coefficients from these models can be post-processed to derive the discriminant coefficients.
#     This allows the idea of linear discriminant analysis to be extended in a number of ways.
#     First, many of the models, such as lasso, ridge regression, or MARS, can be extended to create discriminant variables.
#     For example, MARS can be used to create a set of hinge functions that result in discriminant functions that are nonlinear combinations of the original predictors.
#     This conceptual framework is referred to as flexible discriminant analysis (FDA).
#     Essentially, the MARS features isolate multidimensional polytopal regions of the predictor space and predict a common class within these regions.
#   - Bagging the model coerces FDA to produce smoother relationships between the predictors and the outcome. MARS models are moderately unstable predictors
#     since they use exhaustive searches of the data and the splits are based on specific data points in the training set.
#     Bagging the FDA model will have the effect of adding more splits for the important predictors, leading to a better approximation.
#     However, our experience is that bagging MARS or FDA models has a marginal impact on model performance and increased number of terms diminishes the 
#     interpretation of the discriminant equation.
#   - Since many of the predictors in the FDA model are on different scales, it is difficult to use the discriminant function to uncover which variables have
#     the most impact on the outcome.
# ------------------------------------------------------------------------------

packages <- c("mda", "earth", "nnet", "C50", "ipred", "gbm", "randomForests")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ----------
# Flexible Discriminant Analysis
mfda0 <- fda(formula = type ~ class + density + hardness + size + weight + moisture, data = wheat, method = earth)

mfda0

plot(mfda0)


coef(mfda0)



# ----------
# Support Vector Machines
msvm0 <- ksvm(type ~ class + density + hardness + size + weight + moisture, data = wheat)

msvm0



# ----------
# Neural Networks
mnnet0 <- nnet(type ~ class + density + hardness + size + weight + moisture, data = wheat, size = 5, decay = 0.01, maxit = 200)

summary(mnnet0)

coef(mnnet0)



# ----------
# Random Forests
mrf0 <- randomForest(type ~ class + density + hardness + size + weight + moisture, data = wheat)

plot(mrf0)



# ----------
# C5.0
mc500 <- C5.0(type ~ class + density + hardness + size + weight + moisture, data = wheat, trials = 10, control = C5.0Control(earlyStopping = TRUE), rules = FALSE)

plot(mc500)

summary(mc500)



# ----------
# Gradient Boosted Trees
# wheat$type_int <- as.numeric(wheat$type) - 1
# table(wheat$type_int)
# mgbm0 <- gbm(type_int ~ class + density + hardness + size + weight + moisture, data = wheat, distribution = "multinomial", interaction.depth = 5, n.trees = 1500, shrinkage = 0.01)



# ----------
# Bagged Trees
mbag0 <- bagging(type ~ class + density + hardness + size + weight + moisture, data = wheat)

summary(mbag0)




# ------------------------------------------------------------------------------
# Compare class prediction
# ------------------------------------------------------------------------------

xtabs(~ predict(mod.fit, type = "class") + wheat$type)
xtabs(~ predict(mod.fit.ord, type = "class") + wheat$type)
xtabs(~ predict(mlda)$class + wheat$type)
xtabs(~ predict(mfda0, type = "class") + wheat$type)
xtabs(~ predict(msvm0, type = "response") + wheat$type)
xtabs(~ predict(mnnet0, type = "class") + wheat$type)
xtabs(~ predict(mrf0, type = "class") + wheat$type)
xtabs(~ predict(mc500, type = "class", newdata = wheat[,var_n]) + wheat$type)
xtabs(~ predict(mbag0, type = "class") + wheat$type)
# xtabs(~ predict(mgbm0, type = "response", n.trees = 1500, newdata = wheat[,c(var_n, "type_int")]) + wheat$type)


sum(diag(xtabs(~ predict(mod.fit, type = "class") + wheat$type)))
xtabs(~ predict(mod.fit.ord, type = "class") + wheat$type)
sum(diag(xtabs(~ predict(mlda)$class + wheat$type)))
sum(diag(xtabs(~ predict(mfda0, type = "class") + wheat$type)))
sum(diag(xtabs(~ predict(msvm0, type = "response") + wheat$type)))
sum(diag(xtabs(~ predict(mnnet0, type = "class") + wheat$type)))
sum(diag(xtabs(~ predict(mc500, type = "class", newdata = wheat[,var_n]) + wheat$type)))


# -->
# Support Vector Machines and Neural Networks are best in Accuracy
# Accuracy of C50 seems to be best, but it is just overfitted.

