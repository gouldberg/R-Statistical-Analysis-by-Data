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
# Accumulated Local Effects Plot (ALE plot)
#   - Accumulated local effects describe how features influence the prediction of a machine learning model on average.
#     ALE plots are a faster and unbiased alternative to partial dependence plots (PDPs).
#   - If features of a machine learning model are correlated, the partial dependence plot cannot be trusted.
#     The computation of a partial dependence plot for a feature that is strongly correlated with other features involves averaging predictions of artificial data instances
#     that are unlikely in reality. This can greatly bias the estimated feature effect.
#   - ALE plots calculate – also based on the conditional distribution of the features – differences in predictions instead of averages.
#     For the effect of living area at 30 m2, the ALE method uses all houses with about 30 m2, gets the model predictions pretending these houses were 31 m2 minus the prediction
#     pretending they were 29 m2. This gives us the pure effect of the living area and is not mixing the effect with the effects of correlated features.
#     The use of differences blocks the effect of other features.
#
#   - To summarize how each type of plot (PDP, M, ALE) calculates the effect of a feature at a certain grid value v:
#        - Partial Dependence Plots: “Let me show you what the model predicts on average when each data instance has the value v for that feature. I ignore whether the value v makes sense for all data instances.”
#        - M-Plots: “Let me show you what the model predicts on average for data instances that have values close to v for that feature. The effect could be due to that feature, but also due to correlated features.”
#        - ALE plots: “Let me show you how the model predictions change in a small ”window" of the feature around v for data instances in that window."
#
#   - ALE plots can also show the interaction effect of two features.
#     The calculation principles are the same as for a single feature, but we work with rectangular cells instead of intervals,
#     because we have to accumulate the effects in two dimensions. 
# ------------------------------------------------------------------------------

library("mlr")
library("ggplot2")

set.seed(42)

bike.task = makeRegrTask(data = bike, target = "cnt")

mod.bike = mlr::train(mlr::makeLearner(cl = 'regr.ctree'), bike.task)$learner.model

pred.bike = Predictor$new(mod.bike, data = bike, y = "cnt")



# ----------
# ALE plots for the bike prediction model by temperature, humidity and wind speed.
limits = c(-1500, 800)

ale1 = FeatureEffect$new(pred.bike, "temp", method = "ale")$plot() + scale_x_continuous("Temperature") + scale_y_continuous("ALE", limits = limits)

ale2 = FeatureEffect$new(pred.bike, "hum", method = "ale")$plot() + scale_x_continuous("Humidity") + scale_y_continuous("", limits = limits)

ale3 = FeatureEffect$new(pred.bike, "windspeed", method = "ale")$plot() + scale_x_continuous("Wind speed") + scale_y_continuous("", limits = limits)

gridExtra::grid.arrange(ale1, ale2, ale3, ncol = 3)



# -->
# The temperature has a strong effect on the prediction.
# The average prediction rises with increasing temperature, but falls again above 25 degrees Celsius.
# Humidity has a negative effect: When above 60%, the higher the relative humidity, the lower the prediction.
# The wind speed does not affect the predictions much.



# ------------------------------------------------------------------------------
# Check the correlation between weather features and all other features
#   - Let us look at the correlation between temperature, humidity and wind speed and all other features.
#     Since the data also contains categorical features, we cannot only use the Pearson correlation coefficient,
#     which only works if both features are numerical. Instead, I train a linear model to predict, for example, temperature based on one of the other features as input.
#     Then I measure how much variance the other feature in the linear model explains and take the square root.
#     If the other feature was numerical, then the result is equal to the absolute value of the standard Pearson correlation coefficient.
#     But this model-based approach of “variance-explained” (also called ANOVA, which stands for ANalysis Of VAriance) works even if the other feature is categorical.
#     The “variance-explained” measure lies always between 0 (no association) and 1 (temperature can be perfectly predicted from the other feature). 
#     We calculate the explained variance of temperature, humidity and wind speed with all the other features.
#   - The higher the explained variance (correlation), the more (potential) problems with PD plots.
# ------------------------------------------------------------------------------

mycor = function(cnames, dat) {
  x.num = dat[cnames[1]][[1]]
  x.cat = dat[cnames[2]][[1]]
  av = anova(lm(x.num ~ x.cat))
  sqrt(av$`Sum Sq`[1] / sum(av$`Sum Sq`))
}

cnames = c("temp", "hum", "windspeed")

combs = expand.grid(y = cnames, x = setdiff(colnames(bike), "cnt"))

combs$cor = apply(combs, 1, mycor, dat = bike)

combs$lab = sprintf("%.2f", combs$cor)

forder = c(cnames, setdiff(unique(combs$x), cnames))

combs$x = factor(combs$x, levels = forder)

combs$y = factor(combs$y, levels = rev(cnames))

ggplot(combs, aes(x = x, y = y, fill = cor, label = lab)) + 
  geom_tile() + 
  geom_label(fill = "white", size = 3) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  + 
  scale_x_discrete("") + 
  scale_y_discrete("") + 
  scale_fill_viridis("Variance\nexplained", begin = 0.2)



# -->
# Weather features are strongly correlated with other features.
# For temperature we observe – not surprisingly – a high correlation with season and month.
# Humidity correlates with weather situation. 



# PDPs for temperature, humidity and wind speed. 
pdp = FeatureEffect$new(pred.bike, "temp", method = "pdp") 
p1 = pdp$plot() +  scale_x_continuous('Temperature') + scale_y_continuous('Predicted number of rented bikes', limits = c(3700, 5300))

pdp$set.feature("hum")
p2 = pdp$plot() +  scale_x_continuous('Humidity') + scale_y_continuous('', limits = c(3000, 5500))

pdp$set.feature("windspeed")
p3 = pdp$plot() + scale_x_continuous('Wind speed') + scale_y_continuous('', limits = c(3000, 5500))

gridExtra::grid.arrange(p1, p2, p3, ncol = 3)


# -->
# Compared to the ALE plots, the PDPs show a smaller decrease in predicted number of bikes for high temperature or high humidity.
# The PDP uses all data instances to calculate the effect of high temperatures, even if they are, for example,
# instances with the season "winter". The ALE plots are more reliable.



# ------------------------------------------------------------------------------
# Accumulated Local Effects Plot for categorical feature
# ------------------------------------------------------------------------------

# ALE plot for the categorical feature month. 
alecat1 = FeatureEffect$new(pred.bike, "mnth", method = "ale") 

ggplot(alecat1$results) + 
  geom_col(aes(x = mnth, y = .ale), fill = default_color, width = 0.3) + 
  scale_x_discrete('') + 
  scale_y_continuous("ALE of predicted bike rentals")


# -->
# The months are ordered by their similarity to each other, based on the distributions of the other features by month.
# We observe that January, March and April, but especially December and November, have a lower effect on the predicted number of rented bikes
# compared to the other months.


# Since many of the features are related to weather, the order of the months strongly reflects how similar the weather is between the months.
# All colder months are on the left side (February to April) and the warmer months on the right side (October to August). 
# Keep in mind that non-weather features have also been included in the similarity calculation, for example relative frequency of holidays has the same weight as the temperature for calculating the similarity between the months.



# ------------------------------------------------------------------------------
# Accumulated Local Effects Plot for 2nd-order effect of humidity and temperature
#   - Remember that the second-order effect is the additional interaction effect of the two features and does not include the main effects. 
#     This means that, for example, you will not see the main effect that high humidity leads to a lower number of predicted bikes on average
#     in the second-order ALE plot.
# ------------------------------------------------------------------------------
FeatureEffect$new(pred.bike, feature = c("hum", "temp"), method = "ale", grid.size = 40)$plot() +   
  scale_fill_gradient("ALE", low = "red", high = "yellow") + 
  scale_x_continuous("Relative Humidity") + 
  scale_y_continuous("Temperature")+
  scale_fill_viridis(option = "D")


# -->
# Lighter shade indicates an above average and darker shade a below average prediction
# when the main effects are already taken into account.
# The plot reveals an interaction between temperature and humidity: Hot and humid weather increases the prediction.
# In cold and humid weather an additional negative effect on the number of predicted bikes is shown.

# Keep in mind that both main effects of humidity and temperature say that the predicted number of bikes decreases in very hot and humid weather.
# In hot and humid weather, the combined effect of temperature and humidity is therefore not the sum of the main effects,
# but larger than the sum.
# To emphasize the difference between the pure second-order effect (the 2D ALE plot you just saw) and the total effect,
# let us look at the partial dependence plot. The PDP shows the total effect, which combines the mean prediction,
# the two main effects and the second-order effect (the interaction).



# ----------
# PDP of the total effect of temperature and humidity on the predicted number of bikes.
# The plot combines the main effect of each of the features and their interaction effect,
# as opposed to the 2D-ALE plot which only shows the interaction."}
pdp = FeatureEffect$new(pred.bike, c("hum", "temp"), method = "pdp")

pdp$plot() + 
  scale_fill_gradient("Prediction", low = "red", high = "yellow") + 
  scale_x_continuous("Relative Humidity") + 
  scale_y_continuous("Temperature") +
  scale_fill_viridis(option = "D")



# -->
# If you are only interested in the interaction, you should look at the second-order effects, because the total effect mixes the main effects into the plot.
# But if you want to know the combined effect of the features, you should look at the total effect (which the PDP shows).
# For example, if you want to know the expected number of bikes at 30 degrees Celsius and 80 percent humidity,
# you can read it directly from the 2D PDP.
# If you want to read the same from the ALE plots, you need to look at three plots:
# The ALE plot for temperature, for humidity and for temperature + humidity and you also need to know the overall mean prediction.
# In a scenario where two features have no interaction, the total effect plot of the two features could be misleading
# because it probably shows a complex landscape, suggesting some interaction, but it is simply the product of the two main effects.
# The second-order effect would immediately show that there is no interaction.



# ----------
# Advantages:
# ALE plots are unbiased, which means they still work when features are correlated.
# Partial dependence plots fail in this scenario because they marginalize over unlikely or even physically impossible combinations of feature values.

# ALE plots are faster to compute than PDPs and scale with O(n), since the largest possible number of intervals is the number of instances
# with one interval per instance. The PDP requires n times the number of grid points estimations.
# For 20 grid points, PDPs require 20 times more predictions than the worst case ALE plot where as many intervals as instances are used.

# The interpretation of ALE plots is clear: Conditional on a given value, the relative effect of changing the feature on the prediction can be read from the ALE plot.
# ALE plots are centered at zero. This makes their interpretation nice, because the value at each point of the ALE curve is the difference to the mean prediction.
# The 2D ALE plot only shows the interaction: If two features do not interact, the plot shows nothing.

# All in all, in most situations I would prefer ALE plots over PDPs, because features are usually correlated to some extent.



# ----------
# Disadvantates:
# ALE plots can become a bit shaky (many small ups and downs) with a high number of intervals.
# In this case, reducing the number of intervals makes the estimates more stable,
# but also smoothes out and hides some of the true complexity of the prediction model.
# There is no perfect solution for setting the number of intervals. If the number is too small, the ALE plots might not be very accurate.
# If the number is too high, the curve can become shaky.

# Unlike PDPs, ALE plots are not accompanied by ICE curves.
# For PDPs, ICE curves are great because they can reveal heterogeneity in the feature effect, which means that the effect of a feature looks different for subsets of the data.
# For ALE plots you can only check per interval whether the effect is different between the instances, but each interval has different instances so it is not the same as ICE curves.

# Second-order ALE estimates have a varying stability across the feature space, which is not visualized in any way.
# The reason for this is that each estimation of a local effect in a cell uses a different number of data instances.
# As a result, all estimates have a different accuracy (but they are still the best possible estimates).
# The problem exists in a less severe version for main effect ALE plots.
# The number of instances is the same in all intervals, thanks to the use of quantiles as grid,
# but in some areas there will be many short intervals and the ALE curve will consist of many more estimates.
# But for long intervals, which can make up a big part of the entire curve, there are comparatively fewer instances.
# This happened in the cervical cancer prediction ALE plot for high age for example.

# Second-order effect plots can be a bit annoying to interpret, as you always have to keep the main effects in mind.
# It is tempting to read the heat maps as the total effect of the two features, but it is only the additional effect of the interaction.
# The pure second-order effect is interesting for discovering and exploring interactions, but for interpreting what the effect looks like,
# I think it makes more sense to integrate the main effects into the plot.

# The implementation of ALE plots is much more complex and less intuitive compared to partial dependence plots.

# Even though ALE plots are not biased in case of correlated features, interpretation remains difficult when features are strongly correlated.
# Because if they have a very strong correlation, it only makes sense to analyze the effect of changing both features together and not in isolation.
# This disadvantage is not specific to ALE plots, but a general problem of strongly correlated features.

# If the features are uncorrelated and computation time is not a problem, PDPs are slightly preferable because they are easier to understand
# and can be plotted along with ICE curves.

# The list of disadvantages has become quite long, but do not be fooled by the number of words I use: As a rule of thumb: Use ALE instead of PDP.


