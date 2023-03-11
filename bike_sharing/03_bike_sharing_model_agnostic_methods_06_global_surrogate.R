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
# Gloabal Surrogate
#   - A global surrogate model is an interpretable model that is trained to approximate the predictions of a black box model.
#     We can draw conclusions about the black box model by interpreting the surrogate model. 
#   - Surrogate models are also used in engineering: If an outcome of interest is expensive, time-consuming or otherwise difficult to measure (e.g. because it comes from a complex computer simulation),
#     a cheap and fast surrogate model of the outcome can be used instead.
#     The difference between the surrogate models used in engineering and in interpretable machine learning is that the underlying model is a machine learning model (not a simulation)
#     and that the surrogate model must be interpretable.
#   - The purpose of (interpretable) surrogate models is to approximate the predictions of the underlying model as accurately as possible and
#     to be interpretable at the same time. The idea of surrogate models can be found under different names: Approximation model, metamodel, response surface model, emulator, 
#
#   - Training a surrogate model is a model-agnostic method, since it does not require any information about the inner workings of the black box model,
#     only access to data and the prediction function is necessary. If the underlying machine learning model was replaced with another,
#     you could still use the surrogate method. The choice of the black box model type and of the surrogate model type is decoupled.
#   - Perform the following steps to obtain a surrogate model:
#        - 1.  Select a dataset X. This can be the same dataset that was used for training the black box model or a new dataset from the same distribution.
#          You could even select a subset of the data or a grid of points, depending on your application.
#        - 2.  For the selected dataset X, get the predictions of the black box model.
#        - 3.  Select an interpretable model type (linear model, decision tree, …).
#        - 4.  Train the interpretable model on the dataset X and its predictions.
#        - 5.  Congratulations! You now have a surrogate model.
#        - 6.  Measure how well the surrogate model replicates the predictions of the black box model.
#        - 7.  Interpret the surrogate model.
# ------------------------------------------------------------------------------

# First, we train a support vector machine to predict the daily number of rented bikes given weather and calendar information.
# The support vector machine is not very interpretable, so we train a surrogate with a CART decision tree as interpretable model to approximate the behavior of the support vector machine.  

library("iml")

bike.task = makeRegrTask(data = bike, target = "cnt")

mod.bike = mlr::train(mlr::makeLearner(cl = 'regr.svm'), bike.task)

pred.bike = Predictor$new(mod.bike, data = bike[, names(bike) != "cnt"])

tree = TreeSurrogate$new(pred.bike) 

plot(tree)


# The terminal nodes of a surrogate tree that approximates the predictions of a support vector machine trained on the bike rental dataset.

# The distributions in the nodes show that the surrogate tree predicts a higher number of rented bikes when temperature is above 13 degrees Celsius and
# when the day was later in the 2 year period (cut point at 435 days).

# The surrogate model has a R-squared (variance explained) of 0.77 which means
# it approximates the underlying black box behavior quite well, but not perfectly.
# If the fit were perfect, we could throw away the support vector machine and use the tree instead.

pred.tree  = predict(tree, bike)
head(pred.tree)

pred.svm = getPredictionResponse(predict(mod.bike, bike.task))
head(pred.svm)

  
# The terminal nodes of a surrogate tree that approximates the predictions of a support vector machine trained on the bike rental dataset. The distributions in the nodes show that the surrogate tree predicts a higher number of rented bikes when temperature is above 13 degrees Celsius and when the day was later in the 2 year period (cut point at 435 days)."}
  
  
