setwd("//media//kswada//MyFiles//R//boston")

packages <- c("dplyr", "caret", "MASS", "iml")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Boston
# ------------------------------------------------------------------------------
data("Boston", package = "MASS")

data <- Boston

dim(data)

str(data)

car::some(data)



# ------------------------------------------------------------------------------
# Random Forest Model
# ------------------------------------------------------------------------------
library(randomForest)


rf <- randomForest(medv ~ ., data = data, ntree = 50)
rf

plot(rf)



# ------------------------------------------------------------------------------
# Compute the accumulated local effects for the first feature
#
#  - Accumuluated local effects and partial dependence plots both show the average model predictionover the feature.
#    The difference is that ALE are computed as accumulated differences over the conditional distribution and
#    partial dependence plots over the marginal distribution.
#  - ALE plots preferable to PDPs, because they are faster and unbiased when features are correlated.
#    ALE plots for categorical features are automatically ordered by the similarity of the categories based on the distribution of the other features
#    for instances in a category. When the feature is an ordered factor, the ALE plot leaves the order as is.
#  - Individual conditional expectation curves (ICE curves) describe how, for a single observation, the prediction changes when the feature changes
#    and can be combined with partial dependence plots.
# ------------------------------------------------------------------------------
mod <- Predictor$new(rf, data = data)


# FeatureEffect computes and plots (individual) feature effects of prediction models
# 'ale' for accumulated local effects (the default)
eff <- FeatureEffect$new(mod, feature = "rm", grid.size = 30)
eff$plot()



# ----------
# Again, but this time with a partial dependence plot and ice curves
# 'pdp+ice': partial dependence plot and ice curves within the same plot  (ICE: Individual Conditional Expectation Curves)
eff <- FeatureEffect$new(mod, feature = "rm", method = "pdp+ice", grid.size = 30)

plot(eff)



# Since the result is a ggplot object, you can extend it:
if (require("ggplot2")) {plot(eff) +
    # Adds a title
    ggtitle("Partial dependence") +
    # Adds original predictions
    geom_point(data = data, aes(y = mod$predict(data)[[1]], x = rm), color =  "pink", size = 0.5)}



# If you want to do your own thing, just extract the data:
eff.dat <- eff$results
head(eff.dat)



# ------------------------------------------------------------------------------
# Others
# ------------------------------------------------------------------------------
# You can also use the object to "predict" the marginal values
eff$predict(data[1:3,])


# Instead of the entire data.frame, you can also use feature values
eff$predict(c(5,6,7))


# You can reuse the pdp object for other features
eff$set.feature("lstat")
plot(eff)


# Only plotting the aggregated partial dependence
eff <- FeatureEffect$new(mod, feature = "crim", method = "pdp")
eff$plot()


# Only plotting the individual conditional expectation
eff <- FeatureEffect$new(mod, feature = "crim", method = "ice")
eff$plot()


# Accumulated local effects and partial dependence plots support up to two features
eff <- FeatureEffect$new(mod, feature = c("crim", "lstat"))
plot(eff)



