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
# Model-Agnostic Interpretation Model:
# 
#   - Separating the explanations from the machine learning model (= model-agnostic interpretation methods) has some advantages (Ribeiro, Singh, and Guestrin 201626).
#     The great advantage of model-agnostic interpretation methods over model-specific ones is their flexibility.
#     Machine learning developers are free to use any machine learning model they like when the interpretation methods can be applied to any model.
#     Anything that builds on an interpretation of a machine learning model, such as a graphic or user interface, also becomes independent of the underlying machine learning model.
#     Typically, not just one, but many types of machine learning models are evaluated to solve a task, and when comparing models in terms of interpretability, it is easier to work with model-agnostic explanations,
#     because the same method can be used for any type of model.
# 
#   - An alternative to model-agnostic interpretation methods is to use only interpretable models,
#     which often has the big disadvantage that predictive performance is lost compared to other machine learning models and you limit yourself to one type of model. The other alternative is to use model-specific interpretation methods.
#     The disadvantage of this is that it also binds you to one model type and it will be difficult to switch to something else.
# 
#   - Desirable aspects of a model-agnostic explanation system are (Ribeiro, Singh, and Guestrin 2016):
#       - Model flexibility: The interpretation method can work with any machine learning model, such as random forests and deep neural networks.
#       - Explanation flexibility: You are not limited to a certain form of explanation. In some cases it might be useful to have a linear formula, in other cases a graphic with feature importances.
#       - Representation flexibility: The explanation system should be able to use a different feature representation as the model being explained.
#         For a text classifier that uses abstract word embedding vectors, it might be preferable to use the presence of individual words for the explanation.
# ------------------------------------------------------------------------------



# ------------------------------------------------------------------------------
# Partial Dependence Plot (PDP)
# 
#   - The partial dependence plot (short PDP or PD plot) shows the marginal effect one or two features have on the predicted outcome of a machine learning model (J. H. Friedman 200127).
#     A partial dependence plot can show whether the relationship between the target and a feature is linear, monotonic or more complex. 
#
#   - The partial dependence function for regression is defined as:
#     ^fxS(xS) = ExC[^f(xS,xC)] = ∫ ^f(xS,xC) dP(xC)
#     The xS are the features for which the partial dependence function should be plotted and xC are the other features used in the machine learning model ^f.
#     Usually, there are only one or two features in the set S.
#     The feature(s) in S are those for which we want to know the effect on the prediction.
#     The feature vectors xS and xC combined make up the total feature space x.
#
#   - Partial dependence works by marginalizing the machine learning model output over the distribution of the features in set C,
#     so that the function shows the relationship between the features in set S we are interested in and the predicted outcome.
#     By marginalizing over the other features, we get a function that depends only on features in S, interactions with other features included.
#
#   - The partial function ^fxS is estimated by calculating averages in the training data, also known as Monte Carlo method:
#     ^fxS(xS) = 1 / n * ∑ ^f(xS,x(i)C)
#     The partial function tells us for given value(s) of features S what the average marginal effect on the prediction is.
#     In this formula, x(i)C are actual feature values from the dataset for the features in which we are not interested,
#     and n is the number of instances in the dataset.
#     An assumption of the PDP is that the features in C are not correlated with the features in S.
#     If this assumption is violated, the averages calculated for the partial dependence plot will include data points that are very unlikely or even impossible
#
#   - For classification where the machine learning model outputs probabilities, the partial dependence plot displays the probability for a certain class given different values for feature(s) in S.
#     An easy way to deal with multiple classes is to draw one line or plot per class.
#     The partial dependence plot is a global method: The method considers all instances and gives a statement about the global relationship of a feature with the predicted outcome.
# ------------------------------------------------------------------------------

# the set of features S usually only contains one feature or a maximum of two, 
# because one feature produces 2D plots and two features produce 3D plots.
# Everything beyond that is quite tricky. Even 3D on a 2D paper or monitor is already challenging.

# First we fit a machine learning model, then we analyze the partial dependencies.
# In this case, we have fitted a random forest to predict the number of bicycles and


library("mlr")
library("iml")
library("ggplot2")

bike.task = makeRegrTask(data = bike, target = "cnt")
bike.task



# ----------
# Fit a random forest model
mod.bike = mlr::train(mlr::makeLearner(cl = 'regr.randomForest', id = 'bike-rf'), bike.task)


# predict
pred.bike = Predictor$new(mod.bike, data = bike)



# ----------
pdp = FeatureEffect$new(pred.bike, "temp", method = "pdp") 
pdp
pdp$plot()

p1 = pdp$plot() + scale_x_continuous('Temperature', limits = c(0, NA)) + scale_y_continuous('Predicted number of bikes', limits = c(0, 5500))



# ----------
pdp$set.feature("hum")
pdp$plot()

p2 = pdp$plot() + scale_x_continuous('Humidity', limits = c(0, NA)) + scale_y_continuous('', limits = c(0, 5500))



# ----------
pdp$set.feature("windspeed")
pdp$plot()

p3 = pdp$plot() + scale_x_continuous('Wind speed', limits = c(0, NA)) + scale_y_continuous('', limits = c(0, 5500))



# ----------
# The influence of the weather features on the predicted bike counts is visualized in the following figure.
# PDPs for the bicycle count prediction model and temperature, humidity and wind speed.
gridExtra::grid.arrange(p1, p2, p3, ncol = 3)



# ----------
# The largest differences can be seen in the temperature. The hotter, the more bikes are rented.
# This trend goes up to 20 degrees Celsius, then flattens and drops slightly at 30. Marks on the x-axis indicate the data distribution. 

# For warm but not too hot weather, the model predicts on average a high number of rented bicycles.
# potential bikers are increasingly inhibited in renting a bike when humidity exceeds 60%.
# In addition, the more wind the fewer people like to cycle, which makes sense.
# Interestingly, the predicted number of bike rentals does not fall when wind speed increases from 25 to 35 km/h,
# but there is not much training data, so the machine learning model could probably not learn a meaningful prediction for this range.
# At least intuitively, I would expect the number of bicycles to decrease with increasing wind speed, especially when the wind speed is very high.



# ------------------------------------------------------------------------------
# Partial Dependence Plot (PDP) with a categorical feature
# ------------------------------------------------------------------------------

# PDPs for the bike count prediction model and the season. 
pdp = FeatureEffect$new(pred.bike, "season", method = "pdp") 
pdp$plot()


ggplot(pdp$results) + 
  geom_col(aes(x = season, y = .y.hat), fill = default_color, width = 0.3) + 
  scale_x_discrete('Season') + 
  scale_y_continuous('', limits = c(0, 5500))


# -->
# Unexpectedly all seasons show the same effect, only for spring the model predicts less bicycle rentals.'}



# ----------
# Advantage:
# If the feature for which you computed the PDP is not correlated with the other features, then the PDPs perfectly represent how the feature influences the prediction on average.
# In the uncorrelated case, the interpretation is clear: The partial dependence plot shows how the average prediction in your dataset changes when the j-th feature is changed.
# It is more complicated when features are correlated, see also disadvantages.



# ----------
# Disadvantage:
# The realistic maximum number of features in a partial dependence function is two. This is not the fault of PDPs, but of the 2-dimensional representation (paper or screen) and
# also of our inability to imagine more than 3 dimensions.
# Some PD plots do not show the feature distribution. Omitting the distribution can be misleading, because you might overinterpret regions with almost no data.
# This problem is easily solved by showing a rug (indicators for data points on the x-axis) or a histogram.

# The assumption of independence is the biggest issue with PD plots.
# It is assumed that the feature(s) for which the partial dependence is computed are not correlated with other features.
# For example, suppose you want to predict how fast a person walks, given the person’s weight and height.
# For the partial dependence of one of the features, e.g. height, we assume that the other features (weight) are not correlated with height,
# which is obviously a false assumption.
# For the computation of the PDP at a certain height (e.g. 200 cm), we average over the marginal distribution of weight, which might include a weight below 50 kg, which is unrealistic for a 2 meter person. 
# In other words: When the features are correlated, we create new data points in areas of the feature distribution where the actual probability is very low (for example it is unlikely that
# someone is 2 meters tall but weighs less than 50 kg).
# One solution to this problem is Accumulated Local Effect plots or short ALE plots that work with the conditional instead of the marginal distribution.

# Heterogeneous effects might be hidden because PD plots only show the average marginal effects.
# Suppose that for a feature half your data points have a positive association with the prediction – the larger the feature value the larger the prediction –
# and the other half has a negative association – the smaller the feature value the larger the prediction.
# The PD curve could be a horizontal line, since the effects of both halves of the dataset could cancel each other out.
# You then conclude that the feature has no effect on the prediction. 
# By plotting the individual conditional expectation curves instead of the aggregated line, we can uncover heterogeneous effects.
