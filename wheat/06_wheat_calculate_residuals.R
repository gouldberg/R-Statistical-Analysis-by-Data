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
library(nnet)

mod.fit <- multinom(formula = type ~ class + density + hardness + size + weight + moisture, data = wheat)



# ----------
# Order properly the type factor
levels(wheat$type)
wheat$type.order <- factor(wheat$type, levels = c("Scab", "Sprout", "Healthy"))
levels(wheat$type.order)

library(MASS)

# The method = "logistic" argument value instructs to use the logit transformation on the cumulative probabilities
mod.fit.ord <- polr(formula = type.order ~ class + density + hardness + size + weight + moisture, data = wheat, method = "logistic")
# mod.fit.ord2 <- polr(formula = type.order ~ class + density + hardness + size + weight + moisture, data = wheat)

summary(mod.fit.ord)
# summary(mod.fit.ord2)



# ------------------------------------------------------------------------------
# Residuals
# ------------------------------------------------------------------------------

# multinomial logit model (non-proportional)
head(residuals(mod.fit))



# ----------
# the model object of proportional odds model does not provide residuals information
head(residuals(mod.fit.ord, method = "pearson"))

obs <- data.frame(Scab = ifelse(test = wheat$type.order == "Scab", yes = 1, no = 0), 
                Sprout = ifelse(test = wheat$type.order == "Sprout", yes = 1, no = 0),
                Healthy = ifelse(test = wheat$type.order == "Healthy", yes = 1, no = 0))  # Observed outcomes using 0's and 1's
resid.ord <- obs - predict(object = mod.fit.ord, type = "probs")

head(resid.ord)



