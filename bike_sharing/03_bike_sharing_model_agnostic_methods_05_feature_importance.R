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
# Feature Importance
#   - The importance of a feature is the increase in the prediction error of the model after we permuted the feature’s values, 
#     which breaks the relationship between the feature and the true outcome.
#   - The concept is really straightforward: We measure the importance of a feature by calculating the increase in the model’s prediction error
#     after permuting the feature. A feature is “important” if shuffling its values increases the model error, because in this case the model relied on the feature
#     for the prediction. A feature is “unimportant” if shuffling its values leaves the model error unchanged, because in this case the model ignored the feature for the prediction.
#   - The permutation feature importance measurement was introduced by Breiman (2001) for random forests.
#     Based on this idea, Fisher, Rudin, and Dominici (2018) proposed a model-agnostic version of the feature importance and called it model reliance.
#     They also introduced more advanced ideas about feature importance, for example a (model-specific) version that takes into account that many prediction models may predict the data well.
#     Their paper is worth reading.
# ------------------------------------------------------------------------------
library(mlr)
library(iml)

task = makeRegrTask(data = bike, target = "cnt")


# We fit a support vector machine model to predict the number of rented bikes, given weather conditions and calendar information.
learner = makeLearner('regr.svm')
# learner = makeLearner('regr.nnet')
# learner = makeLearner('regr.h2o.deeplearning', 
#                      par.vals = list(
#                        hidden = c(10, 10),
#                        nesterov_accelerated_gradient = TRUE))


mod = mlr::train(learner, task)

predictor = Predictor$new(mod, data = bike[-which(names(bike) == "cnt")], y = bike$cnt)



# ----------
# As error measurement we use the mean absolute error.
importance = FeatureImp$new(predictor, loss = 'mae')

imp.dat = importance$results



# ----------
# The importance for each of the features in predicting bike counts with a support vector machine.
# The most important feature was temp, the least important was holiday. 
best = which(imp.dat$importance == max(imp.dat$importance))

worst = which(imp.dat$importance == min(imp.dat$importance)) 

plot(importance) + scale_y_discrete("")



# ----------
# Advantage:

# Nice interpretation: Feature importance is the increase in model error when the feature’s information is destroyed.

# Feature importance provides a highly compressed, global insight into the model’s behavior.
# A positive aspect of using the error ratio instead of the error difference is that the feature importance measurements are comparable across different problems.

# The importance measure automatically takes into account all interactions with other features.
# By permuting the feature you also destroy the interaction effects with other features.
# This means that the permutation feature importance takes into account both the main feature effect and the interaction effects on model performance.
# This is also a disadvantage because the importance of the interaction between two features is included in the importance measurements of both features.
# This means that the feature importances do not add up to the total drop in performance, but the sum is larger.
# Only if there is no interaction between the features, as in a linear model, the importances add up approximately.

# Permutation feature importance does not require retraining the model. 
# Some other methods suggest deleting a feature, retraining the model and then comparing the model error. 
# Since the retraining of a machine learning model can take a long time, “only” permuting a feature can save a lot of time.
# Importance methods that retrain the model with a subset of features appear intuitive at first glance, but the model with the reduced data is meaningless for the feature importance.
# We are interested in the feature importance of a fixed model. Retraining with a reduced dataset creates a different model than the one we are interested in.
# Suppose you train a sparse linear model (with Lasso) with a fixed number of features with a non-zero weight.
# The dataset has 100 features, you set the number of non-zero weights to 5. You analyze the importance of one of the features that have a non-zero weight.
# You remove the feature and retrain the model. The model performance remains the same because another equally good feature gets a non-zero weight and your conclusion would be that the feature was not important.
# Another example: The model is a decision tree and we analyze the importance of the feature that was chosen as the first split. 
# You remove the feature and retrain the model. Since another feature is chosen as the first split, the whole tree can be very different,
# which means that we compare the error rates of (potentially) completely different trees to decide how important that feature is for one of the trees.




# ----------
# Disadvantage:

# It is very unclear whether you should use training or test data to compute the feature importance.
# Permutation feature importance is linked to the error of the model. This is not inherently bad, but in some cases not what you need. 
# In some cases, you might prefer to know how much the model’s output varies for a feature without considering what it means for performance.
# For example, you want to find out how robust your model’s output is when someone manipulates the features. In this case, you would not be interested in how much the model performance decreases when a feature is permuted,
# but how much of the model’s output variance is explained by each feature. 
# Model variance (explained by the features) and feature importance correlate strongly when the model generalizes well (i.e. it does not overfit).

# You need access to the true outcome. If someone only provides you with the model and unlabeled data – but not the true outcome – you cannot compute the permutation feature importance.

# The permutation feature importance depends on shuffling the feature, which adds randomness to the measurement.
# When the permutation is repeated, the results might vary greatly. Repeating the permutation and averaging the importance measures over repetitions stabilizes the measure,
# but increases the time of computation.

# If features are correlated, the permutation feature importance can be biased by unrealistic data instances.
# The problem is the same as with partial dependence plots: The permutation of features produces unlikely data instances when two or more features are correlated.
# When they are positively correlated (like height and weight of a person) and I shuffle one of the features, I create new instances that are unlikely or even physically impossible (2 meter person weighing 30 kg for example),
# yet I use these new instances to measure the importance. In other words, for the permutation feature importance of a correlated feature,
# we consider how much the model performance decreases when we exchange the feature with values we would never observe in reality.
# Check if the features are strongly correlated and be careful about the interpretation of the feature importance if they are.

# Another tricky thing: Adding a correlated feature can decrease the importance of the associated feature by splitting the importance between both features.
# Let me give you an example of what I mean by “splitting” feature importance: We want to predict the probability of rain and use the temperature at 8:00 AM of the day
# before as a feature along with other uncorrelated features. I train a random forest and it turns out that the temperature is the most important feature and all is well and
# I sleep well the next night. Now imagine another scenario in which I additionally include the temperature at 9:00 AM as a feature that
# is strongly correlated with the temperature at 8:00 AM. The temperature at 9:00 AM does not give me much additional information
# if I already know the temperature at 8:00 AM. But having more features is always good, right?
# I train a random forest with the two temperature features and the uncorrelated features. Some of the trees in the random forest pick up the 8:00 AM temperature,
# others the 9:00 AM temperature, again others both and again others none.
# The two temperature features together have a bit more importance than the single temperature feature before,
# but instead of being at the top of the list of important features, each temperature is now somewhere in the middle.
# By introducing a correlated feature, I kicked the most important feature from the top of the importance ladder to mediocrity. 
# On one hand this is fine, because it simply reflects the behavior of the underlying machine learning model, here the random forest.
# The 8:00 AM temperature has simply become less important because the model can now rely on the 9:00 AM measurement as well.
# On the other hand, it makes the interpretation of the feature importance considerably more difficult.
# Imagine you want to check the features for measurement errors. The check is expensive and you decide to check only the top 3 of the most important features.
# In the first case you would check the temperature, in the second case you would not include any temperature feature just because they now share the importance.
# Even though the importance values might make sense at the level of model behavior, it is confusing if you have correlated features.
