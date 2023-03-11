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
# Linear Discriminant Analysis
# ------------------------------------------------------------------------------

library(MASS)

mlda0 <- lda(formula = type ~ class + density + hardness + size + weight + moisture, data = wheat)

mlda0



# -->
# The means of the group reveal that:
#   - there is not much difference in classrw, moisture
#   - but there are noticeable differences in density, hardness, size and weight



# ----------
mlda <- update(mlda0, . ~ . - class - moisture)

mlda



# -->
# Trace:  We see that the 1st component is strongly dominant and so the classification will depend mostly on this.



# ------------------------------------------------------------------------------
# Prediction
# ------------------------------------------------------------------------------

preds <- predict(mlda)

head(preds$posterior)



# ----------
# We can get the most likely outcome from each case and compare it against the observed class.
xtabs(~ predict(mlda)$class + wheat$type)



# ------------------------------------------------------------------------------
# Compare class prediction
# ------------------------------------------------------------------------------
preds_mmod <- predict(mod.fit, type = "class")
preds_po <- predict(mod.fit.ord, type = "class")


xtabs(~ preds_mmod + wheat$type)
xtabs(~ preds_po + wheat$type)
xtabs(~ predict(mlda)$class + wheat$type)


# -->
# Accuracy is best by Multinomial Logit Model > Linear Discriminant Model > Proportional Odds Model





