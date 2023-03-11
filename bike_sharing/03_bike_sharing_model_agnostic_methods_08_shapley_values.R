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
# Shapley Values
#   - A prediction can be explained by assuming that each feature value of the instance is a “player” in a game where the prediction is the payout.
#     The Shapley value – a method from coalitional game theory – tells us how to fairly distribute the “payout” among the features.
#   - The Shapley value is the average marginal contribution of a feature value across all possible coalitions.
#     The Shapley value, coined by Shapley (1953), is a method for assigning payouts to players depending on their contribution to the total payout.
#     Players cooperate in a coalition and receive a certain profit from this cooperation.
#     The “game” is the prediction task for a single instance of the dataset.
#     The “gain” is the actual prediction for this instance minus the average prediction for all instances.
#     The “players” are the feature values of the instance that collaborate to receive the gain (= predict a certain value).
#   - The computation time increases exponentially with the number of features.
#     One solution to keep the computation time manageable is to compute contributions for only a few samples of the possible coalitions.
#   - The Shapley value is the only attribution method that satisfies the properties Efficiency, Symmetry, Dummy and Additivity,
#     which together can be considered a definition of a fair payout.
#   - Be careful to interpret the Shapley value correctly:
#     The Shapley value is the average contribution of a feature value to the prediction in different coalitions.
#     The Shapley value is NOT the difference in prediction when we would remove the feature from the model.
# ------------------------------------------------------------------------------

ntree = 30

bike.train.x = bike[names(bike) != 'cnt']

model <- caret::train(bike.train.x, bike$cnt, method = 'rf', ntree=ntree, maximise = FALSE)


# ----------
# For nnet  (nnet is applicable only to single layer)
cvCtrl <- trainControl(method = "repeatedcv", repeats = 3)
model <- caret::train(bike.train.x, bike$cnt, method = 'nnet', preProcess = c("center", "scale"), trControl=cvCtrl, trace = TRUE, maxit = 1000, linout = TRUE)



# ----------
library("iml")

predictor = Predictor$new(model, data = bike.train.x)


# ----------
instance_indices = c(295, 285)

avg.prediction = mean(predict(model))

actual.prediction = predict(model, newdata = bike.train.x[instance_indices[2],])

diff.prediction = actual.prediction - avg.prediction

x.interest = bike.train.x[instance_indices[2],]

shapley2 = Shapley$new(predictor, x.interest = x.interest)



# ----------
plot(shapley2) +  scale_y_continuous("Feature value contribution") +
  ggtitle(sprintf("Actual prediction: %.0f\nAverage prediction: %.0f\nDifference: %.0f", actual.prediction, avg.prediction, diff.prediction))  +
  scale_x_discrete("")


# Shapley values for day 285. With a predicted 2475 rental bikes, this day is -2041 below the average prediction of 4516.
# The weather situation and humidity and had the largest negative contributions.
# The temperature on this day had a positive contribution. The sum of Shapley values yields the difference of actual and average prediction (-2041). 



# ----------
# For neuralnet  (allow multiple hidden layers)
library(neuralnet)

# neuralnet only deals with quantitative variables, you can convert all the qualitative variables (factors) to binary ("dummy") variables or numeric variables, 
bike_s <- rapply(bike, f = as.numeric, classes = "factor", how = c("replace"))
bike_s <- apply(bike_s, FUN = scale, 2) %>% data.frame()


model <- neuralnet(cnt ~ ., data = bike_s, learningrate = 0.05, hidden = c(10, 10), linear.out = TRUE)

library(iml)
predictor = Predictor$new(model, data = bike_s)

instance_indices = c(295, 285)
avg.prediction = mean(predict(model, newdata = bike_s))
actual.prediction = predict(model, newdata = bike_s[instance_indices[2],])
( diff.prediction = actual.prediction - avg.prediction )
x.interest = bike_s[instance_indices[2],]
shapley2 = Shapley$new(predictor, x.interest = x.interest)

plot(shapley2) +  scale_y_continuous("Feature value contribution") +
  ggtitle(sprintf("Actual prediction: %.0f\nAverage prediction: %.0f\nDifference: %.0f", actual.prediction, avg.prediction, diff.prediction))  +
  scale_x_discrete("")




# ----------
# Advantages:
# The difference between the prediction and the average prediction is fairly distributed among the feature values of the instance
# – the Efficiency property of Shapley values. This property distinguishes the Shapley value from other methods such as LIME.
# LIME does not guarantee that the prediction is fairly distributed among the features.
# The Shapley value might be the only method to deliver a full explanation.
# In situations where the law requires explainability – like EU’s “right to explanations” – the Shapley value might be the only legally compliant method,
# because it is based on a solid theory and distributes the effects fairly.

# The Shapley value allows contrastive explanations.
# Instead of comparing a prediction to the average prediction of the entire dataset, you could compare it to a subset or even to a single data point.
# This contrastiveness is also something that local models like LIME do not have.

# The Shapley value is the only explanation method with a solid theory.
# The axioms – efficiency, symmetry, dummy, additivity – give the explanation a reasonable foundation.
# Methods like LIME assume linear behavior of the machine learning model locally, but there is no theory as to why this should work.

# It is mind-blowing to explain a prediction as a game played by the feature values.



# ----------
# Disadvantages:
# The Shapley value requires a lot of computing time.
# In 99.9% of real-world problems, only the approximate solution is feasible.
# An exact computation of the Shapley value is computationally expensive
# because there are 2k possible coalitions of the feature values and the “absence” of a feature has to be simulated by drawing random instances,
# which increases the variance for the estimate of the Shapley values estimation.
# The exponential number of the coalitions is dealt with by sampling coalitions and limiting the number of iterations M.
# Decreasing M reduces computation time, but increases the variance of the Shapley value.
# There is no good rule of thumb for the number of iterations M. M should be large enough to accurately estimate the Shapley values,
# but small enough to complete the computation in a reasonable time. It should be possible to choose M based on Chernoff bounds,
# but I have not seen any paper on doing this for Shapley values for machine learning predictions.

# The Shapley value can be misinterpreted. The Shapley value of a feature value is not the difference of the predicted value
# after removing the feature from the model training.
# The interpretation of the Shapley value is: Given the current set of feature values, the contribution of a feature value to the difference
# between the actual prediction and the mean prediction is the estimated Shapley value.

# The Shapley value is the wrong explanation method if you seek sparse explanations (explanations that contain few features).
# Explanations created with the Shapley value method always use all the features.
# Humans prefer selective explanations, such as those produced by LIME.
# LIME might be the better choice for explanations lay-persons have to deal with.
# Another solution is SHAP introduced by Lundberg and Lee (2016), which is based on the Shapley value,
# but can also provide explanations with few features.

# The Shapley value returns a simple value per feature,
# but no prediction model like LIME. This means it cannot be used to make statements about changes in prediction for changes in the input,
# such as: “If I were to earn €300 more a year, my credit score would increase by 5 points.”

# Another disadvantage is that you need access to the data if you want to calculate the Shapley value for a new data instance.
# It is not sufficient to access the prediction function because you need the data to replace parts of the instance of interest
# with values from randomly drawn instances of the data. This can only be avoided if you can create data instances that look like real data instances
# but are not actual instances from the training data.

# Like many other permutation-based interpretation methods, the Shapley value method suffers from inclusion of unrealistic data instances
# when features are correlated. To simulate that a feature value is missing from a coalition, we marginalize the feature.
# This is achieved by sampling values from the feature’s marginal distribution.
# This is fine as long as the features are independent. When features are dependent, then we might sample feature values that do not make sense for this instance.
# But we would use those to compute the feature’s Shapley value.
# To the best of my knowledge, there is no research on what that means for the Shapley values, nor a suggestion on how to fix it.
# One solution might be to permute correlated features together and get one mutual Shapley value for them.
# Or the sampling procedure might have to be adjusted to account for dependence of features.


# ----------
# SHAP, an alternative formulation of the Shapley values, is implemented in the Python package shap.
# SHAP turns the Shapley values method into an optimization problem and uses a special kernel function to measure proximity of data instances.
# The results of SHAP are sparse (many Shapley values are estimated to be zero), which is the biggest difference from the classic Shapley values.

