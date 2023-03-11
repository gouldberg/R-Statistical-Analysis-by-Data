setwd("//media//kswada//MyFiles//R//bike_sharing")

packages <- c("dplyr", "caret", "lattice", "knitr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# data:  bike sharing  -->  create data by "01_bike_sharing_basics.R" scripts
# ------------------------------------------------------------------------------
bike.features.of.interest <- c('season','holiday', 'workingday', 'weathersit', 'temp', 'hum', 'windspeed', 'days_since_2011')

X = bike[bike.features.of.interest]

y = bike[,'cnt']

dat = cbind(X, y)



# ------------------------------------------------------------------------------
# set my theme for visualization
# ------------------------------------------------------------------------------
library("ggplot2")
library("viridis")

# define graphics theme
my_theme = function(legend.position='right'){
  theme_bw() %+replace%
    theme(legend.position=legend.position)
}

theme_set(my_theme())

default_color = "azure4"



# ------------------------------------------------------------------------------
# Individual Conditional Expectation (ICE) plot
#   - Individual Conditional Expectation (ICE) plots display one line per instance that shows how the instance’s prediction changes when a feature changes.
#   - The partial dependence plot for the average effect of a feature is a global method because it does not focus on specific instances, but on an overall average.
#     The equivalent to a PDP for individual data instances is called individual conditional expectation (ICE) plot (Goldstein et al. 201729).
#   - An ICE plot visualizes the dependence of the prediction on a feature for each instance separately, resulting in one line per instance, compared to one line overall in partial dependence plots.
#     A PDP is the average of the lines of an ICE plot. The values for a line (and one instance) can be computed by keeping all other features the same,
#     creating variants of this instance by replacing the feature’s value with values from a grid and making predictions with the black box model for these newly created instances.
#     The result is a set of points for an instance with the feature value from the grid and the respective predictions.
#   - Partial dependence plots can obscure a heterogeneous relationship created by interactions.
#     PDPs can show you what the average relationship between a feature and the prediction looks like. 
#     This only works well if the interactions between the features for which the PDP is calculated and the other features are weak.
#     In case of interactions, the ICE plot will provide much more insight.
# ------------------------------------------------------------------------------


bike.subset.index = sample(1:nrow(bike), size = 300)

bike.subset = bike[bike.subset.index,]

bike.task = makeRegrTask(data = bike, target = "cnt")



# ----------
mod.bike = mlr::train(mlr::makeLearner(cl = 'regr.randomForest', id = 'bike-rf'), bike.task)

pred.bike = Predictor$new(mod.bike, bike.subset)



# ----------
# ICE plots of predicted bicycle rentals by weather conditions
p1 = FeatureEffect$new(pred.bike, "temp", method = "ice")$plot() + scale_x_continuous("Temperature") + scale_y_continuous("Predicted bicycle rentals")
p2 = FeatureEffect$new(pred.bike, "hum", method = "ice")$plot() + scale_x_continuous("Humidity") + scale_y_continuous("")
p3 = FeatureEffect$new(pred.bike, "windspeed", method = "ice")$plot() + scale_x_continuous("Windspeed")+ scale_y_continuous("")


gridExtra::grid.arrange(p1, p2, p3, ncol = 3)



# -->
# The same effects can be observed as in the partial dependence plots.
# All curves seem to follow the same course, so there are no obvious interactions.
# That means that the PDP is already a good summary of the relationships between the displayed features and the predicted number of bicycles



# ------------------------------------------------------------------------------
# Centered ICE plot
#   - There is a problem with ICE plots: Sometimes it can be hard to tell whether the ICE curves differ between individuals
#     because they start at different predictions. A simple solution is to center the curves at a certain point in the feature and
#     display only the difference in the prediction to this point. The resulting plot is called centered ICE plot (c-ICE).
#   - Anchoring the curves at the lower end of the feature is a good choice.
# ------------------------------------------------------------------------------

bike.subset.index = sample(1:nrow(bike), size = 100)

bike.subset = bike[bike.subset.index,]

predictor = Predictor$new(mod.bike, data = bike.subset)



# ----------
# Centered ICE plot
ice1 = FeatureEffect$new(predictor, feature = "temp", center.at = min(bike$temp), method = "pdp+ice")$plot() 
ice2 = FeatureEffect$new(predictor, feature = "hum", center.at = min(bike$hum), method = "pdp+ice")$plot() 
ice3 = FeatureEffect$new(predictor, feature = "windspeed", center.at = min(bike$windspeed), method = "pdp+ice")$plot() 

gridExtra::grid.arrange(ice1, ice2, ice3, nrow = 1)


# -->
# The lines show the difference in prediction compared to the prediction with the respective feature value at its observed minimum. 




# ------------------------------------------------------------------------------
# Derivative ICE plot
#   - Another way to make it visually easier to spot heterogeneity is to look at the individual derivatives of the prediction function
#     with respect to a feature. The resulting plot is called the derivative ICE plot (d-ICE).
#     The derivatives of a function (or curve) tell you whether changes occur and in which direction they occur.
#     With the derivative ICE plot, it is easy to spot ranges of feature values where the black box predictions change for (at least some) instances.
#   - Without interactions, the individual partial derivatives should be the same for all instances.
#     If they differ, it is due to interactions and it becomes visible in the d-ICE plot.
#   - In addition to displaying the individual curves for the derivative of the prediction function with respect to the feature in S,
#     showing the standard deviation of the derivative helps to highlight regions in feature in S with heterogeneity in the estimated derivatives.
#     The derivative ICE plot takes a long time to compute and is rather impractical.
# ------------------------------------------------------------------------------




# -->
# ICE curves can only display one feature meaningfully, because two features would require the drawing of several overlaying surfaces and
# you would not see anything in the plot.
# ICE curves suffer from the same problem as PDPs: If the feature of interest is correlated with the other features,
# then some points in the lines might be invalid data points according to the joint feature distribution.

