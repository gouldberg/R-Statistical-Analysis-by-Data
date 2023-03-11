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
# Visualize non-linear relationships by 3 techniques
#
# You can model nonlinear relationships using one of the following techniques:
#   - Feature Transformation:  Simple transformation of the feature (e.g. logarithm)
#   - Feature Categorization:  Categorization of the feature
#   - Generalized Additive Models (GAMs)
# ------------------------------------------------------------------------------
# The world is not linear. Linearity in linear models means that no matter what value an instance has in a particular feature,
# increasing the value by one unit always has the same effect on the predicted outcome.
# Is it reasonable to assume that increasing the temperature by one degree at 10 degrees Celsius has the same effect
# on the number of rental bikes as increasing the temperature when it already has 40 degrees ?
# Intuitively, one expects that increasing the temperature from 10 to 11 degrees Celsius has a positive effect on bicycle rentals and from 40 to 41 a negative effect.
# The temperature feature has a linear, positive effect on the number of rental bikes, but at some point it flattens out and even has a negative effect at high temperatures.
# The linear model does not care, it will dutifully find the best linear plane (by minimizing the Euclidean distance).

# Before I go into the details of each method, let us start with an example that illustrates all three of them.
# I took the bike rental dataset and trained a linear model with only the temperature feature to predict the number of rental bikes.
# The following figure shows the estimated slope with: the standard linear model, a linear model with transformed temperature (logarithm),
# a linear model with temperature treated as categorical feature and using regression splines (GAM).

# ----------
mod.simpel = lm(cnt ~ temp, data = bike)
bike.plt = bike
bike.plt$pred.lm = predict(mod.simpel)
bike.plt$log.temp = log(bike$temp + 10)


# ----------
mod.simpel = lm(cnt ~ log.temp, data = bike.plt)
bike.plt$pred.sqrt = predict(mod.simpel)
bike.plt$cat.temp = cut(bike$temp, breaks = seq(from = min(bike$temp), to = max(bike$temp), length.out = 10), include.lowest = TRUE)
mod.simpel = lm(cnt ~ cat.temp, data = bike.plt)
bike.plt$pred.cat = predict(mod.simpel)


# ----------
library(mgcv)
mod.gam = gam(cnt ~ s(temp), data = bike)
bike.plt$pred.gam = predict(mod.gam)



# ----------
bike.plt = data.table::melt(bike.plt[c("pred.lm", "pred.sqrt", "pred.cat", "pred.gam")])
bike.plt$temp = rep(bike$temp, times = 4)
bike.plt$cnt = rep(bike$cnt, times = 4)

model.type = c(pred.lm = "Linear model", 
               pred.sqrt = "Linear model with log(temp + 10)", 
               pred.cat = "Linear model with categorized temp", 
               pred.gam = "GAM")
ggplot(bike.plt) + 
  geom_point(aes(x = temp, y = cnt), size = 1 , alpha = 0.3)  + 
  geom_line(aes(x = temp, y = value), size = 1.2, color = "blue") + 
  facet_wrap("variable", labeller = labeller(variable = model.type)) + 
  scale_x_continuous("Temperature (temp)") + 
  scale_y_continuous("(Predicted) Number of rented bikes")



# -->
# Predicting the number of rented bicycles using only the temperature feature. A linear model (top left) does not fit the data well.
# One solution is to transform the feature with e.g. the logarithm (top right), categorize it (bottom left), which is usually a bad decision, 
# or use Generalized Additive Models that can automatically fit a smooth curve for temperature (bottom right)


# -----------
# Feature transformation
# Often the logarithm of the feature is used as a transformation. Using the logarithm indicates that every 10-fold temperature increase has the same linear effect on the number of bikes,
# so changing from 1 degree Celsius to 10 degrees Celsius has the same effect as changing from 0.1 to 1 (sounds wrong).
# Other examples for feature transformations are the square root, the square function and the exponential function.

# When you use a GLM with a link function that is not the identity function, then the interpretation gets more complicated,
# because you have to incorporate both transformations into the interpretation
# (except when they cancel each other out, like log and exp, then the interpretation gets easier)


# ----------
# Feature categorization
# Another possibility to achieve a nonlinear effect is to discretize the feature; turn it into a categorical feature.
# For example, you could cut the temperature feature into 20 intervals with the levels [-10, -5), [-5, 0), … and so on. When you use the categorized temperature instead of the continuous temperature, the linear model would estimate a step function because each level gets its own estimate. The problem with this approach is that it needs more data, it is more likely to overfit and it is unclear how to discretize the feature meaningfully (equidistant intervals or quantiles? how many intervals?).
# I would only use discretization if there is a very strong case for it. For example, to make the model comparable to another study.



# ----------
# Generalized Additive Models (GAMs)
# Why not ‘simply’ allow the (generalized) linear model to learn nonlinear relationships ?
# That is the motivation behind GAMs. GAMs relax the restriction that the relationship must be a simple weighted sum, and instead assume that the outcome can be modeled by a sum of arbitrary functions of each feature.
# The core of a GAM is still a sum of feature effects, but you have the option to allow nonlinear relationships between some features and the output.
# Linear effects are also covered by the framework, because for features to be handled linearly, you can limit their fj(xj) only to take the form of xjβj.

# The big question is how to learn nonlinear functions. The answer is called “splines” or “spline functions”. 
# Splines are functions that can be combined in order to approximate arbitrary functions. A bit like stacking Lego bricks to build something more complex.
# There is a confusing number of ways to define these spline functions.
# If you are interested in learning more about all the ways to define splines, I wish you good luck on your journey.
# What personally helped me the most for understanding splines was to visualize the individual spline functions and to look into how the data matrix is modified.
# For example, to model the temperature with splines, we remove the temperature feature from the data and replace it with, say, 4 columns, each representing a spline function.
# Usually you would have more spline functions, I only reduced the number for illustration purposes.
# The value for each instance of these new spline features depends on the instances’ temperature values.
# Together with all linear effects, the GAM then also estimates these spline weights. GAMs also introduce a penalty term for the weights to keep them close to zero.
# This effectively reduces the flexibility of the splines and reduces overfitting. A smoothness parameter that is commonly used to control the flexibility of the curve is then tuned via cross-validation.
# Ignoring the penalty term, nonlinear modeling with splines is fancy feature engineering.

# In the example where we are predicting the number of bicycles with a GAM using only the temperature, the model feature matrix looks like this:


mod.gam = gam(cnt ~ s(temp, k = 5), data = bike)
kable(head(model.matrix(mod.gam)), digits = 2)


# --> 
# Each row represents an individual instance from the data (one day).
# Each spline column contains the value of the spline function at the particular temperature values. 
# The following figure shows how these spline functions look like:
  
# To smoothly model the temperature effect, we use 4 spline functions.
# Each temperature value is mapped to (here) 4 spline values.
# If an instance has a temperature of 30 °C, the value for the first spline feature is -1, for the second 0.7, for the third -0.8 and for the 4th 1.7.
mm = model.matrix(mod.gam)

mm2 = data.table::melt(mm)

mm2 = mm2[mm2$Var2 != "(Intercept)",]

ggplot(mm2) + geom_line(aes(x = rep(bike$temp, times = 4), y = value)) + facet_wrap("Var2") + 
  scale_x_continuous("Temperature") + 
  scale_y_continuous("Value of spline feature")


# The GAM assigns weights to each temperature spline feature:
kable(coef(mod.gam), digits = 2, col.names = "weight")


# And the actual curve, which results from the sum of the spline functions weighted with the estimated weights, looks like this:
# GAM feature effect of the temperature for predicting the number of rented bikes (temperature used as the only feature)
plot(mod.gam)


# The interpretation of smooth effects requires a visual check of the fitted curve.
# Splines are usually centered around the mean prediction, so a point on the curve is the difference to the mean prediction.
# For example, at 0 degrees Celsius, the predicted number of bicycles is 3000 lower than the average prediction.


