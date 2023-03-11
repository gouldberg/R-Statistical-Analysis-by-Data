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
# Local Surrogate
#   - Local surrogate models are interpretable models that are used to explain individual predictions of black box machine learning models.
#     Local interpretable model-agnostic explanations (LIME) is a paper in which the authors propose a concrete implementation of local surrogate models.
#     Surrogate models are trained to approximate the predictions of the underlying black box model.
#     Instead of training a global surrogate model, LIME focuses on training local surrogate models to explain individual predictions.
#   - The idea is quite intuitive. First, forget about the training data and imagine you only have the black box model where you can input data points and get the predictions of the model.
#     You can probe the box as often as you want. Your goal is to understand why the machine learning model made a certain prediction.
#     LIME tests what happens to the predictions when you give variations of your data into the machine learning model.
#     LIME generates a new dataset consisting of permuted samples and the corresponding predictions of the black box model.
#     On this new dataset LIME then trains an interpretable model, which is weighted by the proximity of the sampled instances to the instance of interest.
#     The interpretable model can be anything from the interpretable models, for example Lasso or a decision tree.
#     The learned model should be a good approximation of the machine learning model predictions locally, but it does not have to be a good global approximation.
#     This kind of accuracy is also called local fidelity.
#
#   - The recipe for training local surrogate models:
#        - Select your instance of interest for which you want to have an explanation of its black box prediction.
#        - Perturb your dataset and get the black box predictions for these new points.
#        - Weight the new samples according to their proximity to the instance of interest.
#        - Train a weighted, interpretable model on the dataset with the variations.
#        - Explain the prediction by interpreting the local model.
# ------------------------------------------------------------------------------

# After taking into account the trend that the bicycle rental has become more popular over time, we want to know on a certain day whether the number of bicycles rented will be above or below the trend line.
# You can also interpret "above" as being above the average number of bicycles, but adjusted for the trend.
  
ntree = 100

bike.train.resid = factor(resid(lm(cnt ~ days_since_2011, data = bike)) > 0, levels = c(FALSE, TRUE), labels = c('below', 'above'))

bike.train.x = bike[names(bike) != 'cnt']


# First we train a random forest with `ntree` trees on the classification task.
model <- caret::train(bike.train.x, bike.train.resid, method = 'rf', ntree=ntree, maximise = FALSE)



# ----------
# The explanations are created with 2 features.
# The results of the sparse local linear models trained for two instances with different predicted classes:
library("iml")
library("gridExtra")

n_features_lime = 2
instance_indices = c(295, 8)

bike.train.x$temp = round(bike.train.x$temp, 2)

set.seed(44)
pred = Predictor$new(model, data = bike.train.x, class = "above", type = "prob")



# ----------
# LIME explanations for two instances of the bike rental dataset.
lim1 = LocalModel$new(pred, x.interest = bike.train.x[instance_indices[1],], k = n_features_lime)
lim2= LocalModel$new(pred, x.interest = bike.train.x[instance_indices[2],], k = n_features_lime)
wlim = c(min(c(lim1$results$effect, lim2$results$effect)), max(c(lim1$results$effect, lim2$results$effect)))
a = plot(lim1) + scale_y_continuous(limit = wlim) + geom_hline(aes(yintercept=0)) + theme(axis.title.y=element_blank(), axis.ticks.y=element_blank())
b = plot(lim2) + scale_y_continuous(limit = wlim) + geom_hline(aes(yintercept=0)) + theme(axis.title.y=element_blank(), axis.ticks.y=element_blank())

grid.arrange(a, b, ncol = 1)



# -->
# Warmer temperature and good weather situation have a positive effect on the prediction.
# The x-axis shows the feature effect: The weight times the actual feature value. 
# From the figure it becomes clear that it is easier to interpret categorical features than numerical features.
# One solution is to categorize the numerical features into bins.



# ----------
# Advantage:
# Even if you replace the underlying machine learning model, you can still use the same local, interpretable model for explanation.
# Suppose the people looking at the explanations understand decision trees best. Because you use local surrogate models, you use decision trees as explanations without actually having to use a decision tree to make the predictions.
# For example, you can use a SVM. And if it turns out that an xgboost model works better, you can replace the SVM and still use as decision tree to explain the predictions.

# LIME is one of the few methods that works for tabular data, text and images.

# The fidelity measure (how well the interpretable model approximates the black box predictions) gives us a good idea of how reliable the interpretable model is in explaining the black box predictions in the neighborhood of the data instance of interest.

# The explanations created with local surrogate models can use other features than the original model.
# This can be a big advantage over other methods, especially if the original features cannot bet interpreted.
# A text classifier can rely on abstract word embeddings as features, but the explanation can be based on the presence or absence of words in a sentence.
# A regression model can rely on a non-interpretable transformation of some attributes, but the explanations can be created with the original attributes.



# ----------
# Disadvantage:
# The correct definition of the neighborhood is a very big, unsolved problem when using LIME with tabular data.
# In my opinion it is the biggest problem with LIME and the reason why I would recommend to use LIME only with great care.
# For each application you have to try different kernel settings and see for yourself if the explanations make sense.
# Unfortunately, this is the best advice I can give to find good kernel widths.

# Sampling could be improved in the current implementation of LIME.
# Data points are sampled from a Gaussian distribution, ignoring the correlation between features.
# This can lead to unlikely data points which can then be used to learn local explanation models.

# The complexity of the explanation model has to be defined in advance. This is just a small complaint, because in the end the user always has to define the compromise between fidelity and sparsity.

# Another really big problem is the instability of the explanations.
# In an article 38 the authors showed that the explanations of two very close points varied greatly in a simulated setting.
# Also, in my experience, if you repeat the sampling process, then the explantions that come out can be different.
# Instability means that it is difficult to trust the explanations, and you should be very critical.

