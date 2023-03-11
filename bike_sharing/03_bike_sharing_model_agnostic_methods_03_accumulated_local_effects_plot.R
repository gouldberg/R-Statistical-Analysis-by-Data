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
# Feature Interation
#   - One way to estimate the interaction strength is to measure how much of the variation of the prediction depends on the interaction of the features.
#     This measurement is called H-statistic, introduced by Friedman and Popescu (2008)
#   - Friedman and Popescu also propose a test statistic to evaluate whether the H-statistic differs significantly from zero.
#     The null hypothesis is the absence of interaction. To generate the interaction statistic under the null hypothesis, you must be able to adjust the model
#     so that it has no interaction between feature j and k or all others. This is not possible for all types of models.
#     Therefore this test is model-specific, not model-agnostic, and as such not covered here.
# ------------------------------------------------------------------------------

library("mlr")
library("iml")
library("ggplot2")

bike.task = makeRegrTask(data = bike, target = "cnt")

mod.bike = mlr::train(mlr::makeLearner(cl = 'regr.svm', id = 'bike-rf'), bike.task)

pred.bike = Predictor$new(mod.bike, data = bike[setdiff(colnames(bike), "cnt")])

ia = Interaction$new(pred.bike, grid.size = 50) 

plot(ia) + scale_y_discrete("")



# -->
# The interaction strength (H-statistic) for each feature with all other features for a support vector machine predicting bicycle rentals.
# Overall, the interaction effects between the features are very weak (below 10% of variance explained per feature). 



# ----------
# Advantages:
# The interaction H-statistic has an underlying theory through the partial dependence decomposition.

# The H-statistic has a meaningful interpretation: The interaction is defined as the share of variance that is explained by the interaction.

# Since the statistic is dimensionless and always between 0 and 1, it is comparable across features and even across models.

# The statistic detects all kinds of interactions, regardless of their particular form.

# With the H-statistic it is also possible to analyze arbitrary higher interactions such as the interaction strength between 3 or more features.



# ----------
# Disadvantages:
# The first thing you will notice: The interaction H-statistic takes a long time to compute, because it is computationally expensive.

# The computation involves estimating marginal distributions. These estimates also have a certain variance if we do not use all data points.
# This means that as we sample points, the estimates also vary from run to run and the results can be unstable.
# I recommend repeating the H-statistic computation a few times to see if you have enough data to get a stable result.

# It is unclear whether an interaction is significantly greater than 0. We would need to conduct a statistical test,
# but this test is not (yet) available in a model-agnostic version.

# Concerning the test problem, it is difficult to say when the H-statistic is large enough for us to consider an interaction “strong”.

# The H-statistic tells us the strength of interactions, but it does not tell us how the interactions look like.
# That is what partial dependence plots are for.
# A meaningful workflow is to measure the interaction strengths and then create 2D-partial dependence plots for the interactions you are interested in.

# The H-statistic cannot be used meaningfully if the inputs are pixels. So the technique is not useful for image classifier.

# The interaction statistic works under the assumption that we can shuffle features independently.
# If the features correlate strongly, the assumption is violated and we integrate over feature combinations that are very unlikely in reality.
# That is the same problem that partial dependence plots have. You cannot say in general if it leads to overestimation or underestimation.

# Sometimes the results are strange and for small simulations do not yield the expected results. But this is more of an anecdotal observation.



