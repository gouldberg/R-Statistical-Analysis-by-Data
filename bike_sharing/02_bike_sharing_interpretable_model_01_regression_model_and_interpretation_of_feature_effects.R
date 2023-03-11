setwd("//media//kswada//MyFiles//R//bike_sharing")

packages <- c("dplyr", "caret", "lattice", "knitr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# data:  bike sharing  -->  create data by "01_bike_sharing_basics.R" scripts
# ------------------------------------------------------------------------------


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
# linear regression model
# ------------------------------------------------------------------------------
bike.features.of.interest <- c('season','holiday', 'workingday', 'weathersit', 'temp', 'hum', 'windspeed', 'days_since_2011')

X = bike[bike.features.of.interest]

y = bike[,'cnt']

dat = cbind(X, y)

mod = lm(y ~ ., data = dat, x = TRUE)

summary(mod)



# ----------
( lm_summary = summary(mod)$coefficients )
lm_summary_print = lm_summary
lm_summary_print[,'t value'] = abs(lm_summary_print[,'t value'])

# Make character vector pretty
pretty_rownames = function(rnames){
  rnames = gsub('^`', '', rnames)
  rnames = gsub('`$', '', rnames)
  rnames = gsub('`', ':', rnames)
  rnames
}

rownames(lm_summary_print)
( rownames(lm_summary_print) = pretty_rownames(rownames(lm_summary_print)) )

kable(lm_summary_print[,c('Estimate', 'Std. Error', 't value')], digits = 1, col.names = c('Weight', 'SE', '|t|'))



# ------------------------------------------------------------------------------
# Interpretation of a feature
# ------------------------------------------------------------------------------
# Interpretation of a numerical feature (temperature):
# when all other features remain fixed, an increase of the temperature by 1 degree Celsius increases the predicted number of bicycles by
sprintf('%.1f', lm_summary_print['temp', 'Estimate'])



# ----------
# Interpretation of a categorical feature ("weathersit"):
# The estimated number of bicycles is
sprintf('%.1f', lm_summary_print['weathersitRAIN/SNOW/STORM', 'Estimate'])
# lower when it is raining, snowing or stormy, compared to good weather -- again assuming that all other features do not change.

# When the weather is misty, the predicted number of bicycles is
sprintf('%.1f', lm_summary_print['weathersitMISTY', 'Estimate'])
# lower compared to good weather, given all other features remain the same.


# All the interpretations always come with the footnote that "all other features remain the same".
# This is because of the nature of linear regression models.
# The predicted target is a linear combination of the weighted features.
# The estimated linear equation is a hyperplane in the feature/target space (a simple line in the case of a single feature).
# The weights specify the slope (gradient) of the hyperplane in each direction.

# The good side is that the additivity isolates the interpretation of an individual feature effect from all other features.
# That is possible because all the feature effects (= weight times feature value) in the equation are combined with a plus. 
# On the bad side of things, the interpretation ignores the joint distribution of the features.
# Increasing one feature, but not changing another, can lead to unrealistic or at least unlikely data points.
# For example increasing the number of rooms might be unrealistic without also increasing the size of a house.



# ------------------------------------------------------------------------------
# Weight Plot:  Plot coefficients for a linear model (95% confidence intervals)
# ------------------------------------------------------------------------------
# The information of the weight table (weight and variance estimates) can be visualized in a weight plot.

# Plot coefficients of a linear model
coef_plot = function(mod, alpha = 0.05, remove_intercept = TRUE){
  lm_summary = summary(mod)$coefficients
  rownames(lm_summary) = pretty_rownames(rownames(lm_summary))
  
  df = data.frame(Features = rownames(lm_summary),
                  Estimate = lm_summary[,'Estimate'],
                  std_error = lm_summary[,'Std. Error'])
  df$lower = df$Estimate - qnorm(alpha/2) * df$std_error
  df$upper = df$Estimate + qnorm(alpha/2) * df$std_error
  
  
  if(remove_intercept){
    df = df[!(df$Features == '(Intercept)'),]
  }
  require("ggplot2")
  ggplot(df) +
    geom_vline(xintercept=0, linetype=4) +
    geom_point(aes(x=Estimate, y=Features)) +
    geom_segment(aes(y=Features, yend=Features, x=lower, xend=upper), arrow = arrow(angle=90, ends='both', length = unit(0.1, 'cm'))) +
    scale_x_continuous('Weight estimate') +
    my_theme()
}



# Weights are displayed as points and the 95% confidence intervals as lines
coef_plot(mod) + scale_y_discrete("")



# The weight plot shows that rainy/snowy/stormy weather has a strong negative effect on the predicted number of bikes.
# The weight of the working day feature is close to zero and zero is included in the 95% interval, which means that the effect is not statistically significant.
# Some confidence intervals are very short and the estimates are close to zero, yet the feature effects were statistically significant.
# Temperature is one such candidate.

# The problem with the weight plot is that the features are measured on different scales.
# While for the weather the estimated weight reflects the difference between good and rainy/stormy/snowy weather, for temperature it only reflects an increase of 1 degree Celsius.
# You can make the estimated weights more comparable by scaling the features (zero mean and standard deviation of one) before fitting the linear model.




# ------------------------------------------------------------------------------
# Effect Plot
#    - The effect plot can help you understand how much the combination of weight and feature contributes to the predictions in your data.
# ------------------------------------------------------------------------------
# The weights of the linear regression model can be more meaningfully analyzed when they are multiplied by the actual feature values.
# The weights depend on the scale of the features and will be different if you have a feature that measures e.g. a person's height and you switch from meter to centimeter.
# The weight will change, but the actual effects in your data will not.
# It is also important to know the distribution of your feature in the data, because if you have a very low variance, it means that almost all instances have similar contribution from this feature.

# The effects can be visualized with boxplots.
# A box in a boxplot contains the effect range for half of your data (25% to 75% effect quantiles).
# The vertical line in the box is the median effect, i.e. 50% of the instances have a lower and the other half a higher effect on the prediction.
# The horizontal lines extend to +-1.5 * IQR / sqrt(n) with IQR being the inter quartile range (75% quantile minus 25% quantile).
# The dots are outliers.
# The categorical feature effects can be summarized in a single boxplot, compared to the weight plot, where each category has its own row.



# The feature effect plot shows the distribution of effects (= feature value times feature weight) across the data per feature.
# Plot effects of linear model
effect_plot = function(mod, dat,  feature_names=NULL){
  X = get_effects(mod, dat)
  if(!missing(feature_names)){
    rownames(X) = feature_names
  }
  X = tidyr::gather(X)
  require("ggplot2")
  ggplot(X) +
    geom_hline(yintercept=0, linetype=4) +
    geom_boxplot(aes(x=key, y=value, group=key)) +
    coord_flip() +
    scale_y_continuous('Feature effect') +
    my_theme()
}

get_reference_dataset = function(dat){
  df = lapply(dat, function(feature){
    if(class(feature) == 'factor'){
      factor(levels(feature)[1], levels = levels(feature))
    } else {
      0
    }
  })
  data.frame(df)
}

get_effects = function(mod, dat){
  
  X = data.frame(predict(mod, type = 'terms'))
  
  # Nicer colnames
  colnames(X) = gsub('^X\\.', '', colnames(X))
  colnames(X) = gsub('\\.', ' ', colnames(X))
  
  # predict with type='terms' centers the results, so we have to add the mean again
  reference_X = predict(mod, newdata=get_reference_dataset(dat), type='terms')
  X_star = data.frame(t(apply(X, 1, function(x){ x - reference_X[1,names(X)]})))
  X_star
}

effect_plot(mod, dat) + scale_x_discrete("")



# The largest contributions to the expected number of rented bicycles comes from the temperature feature and the days feature, which captures the trend of bike rentals over time.
# The temperature has a broad range of how much it contributes to the prediction.
# The day trend feature goes from zero to large positive contributions, because the first day in the dataset (01.01.2011) has a very small trend effect and the estimated weight for this feature is positive (`r sprintf('%.2f', lm_summary_print['days_since_2011', 'Estimate'])`).
# This means that the effect increases with each day and is highest for the last day in the dataset (31.12.2012).

# Note that for effects with a negative weight, the instances with a positive effect are those that have a negative feature value.
# For example, days with a high negative effect of windspeed are the ones with high wind speeds.



# ------------------------------------------------------------------------------
# Explain Individual Predictions
# ------------------------------------------------------------------------------
# How much has each feature of an instance contributed to the prediction ?
# This can be answered by computing the effects for this instance.
# An interpretation of instance-specific effects only makes sense in comparison to the distribution of the effect for each feature.
# We want to explain the prediction of the linear model for the 6-th instance from the bicycle dataset.
# The instance has the following feature values.
df = data.frame(feature = colnames(bike), value = t(bike[i,]))
colnames(df) = c("feature", "value")
kable(df, col.names = c("Feature", "Value"), row.names = FALSE)



# To obtain the feature effects of this instance, we have to multiply its feature values by the corresponding weights from the linear regression model. 
# For the value “WORKING DAY” of feature “workingday”, the effect is, 124.9. 
# For a temperature of 1.6 degrees Celsius, the effect is 177.6. 
# We add these individual effects as crosses to the effect plot, which shows us the distribution of the effects in the data. 
# This allows us to compare the individual effects with the distribution of effects in the data.

effects = get_effects(mod, dat)
head(effects)

predictions = predict(mod)
head(predictions)

i = 6
effects_i = tidyr::gather(effects[i, ])
predictions_mean = mean(predictions)

# For proper indexing, names have to be removed
names(predictions) = NULL
pred_i = predictions[i]


# ----------
effect_plot(mod, dat) +
  geom_point(aes(x=key, y=value), color = 'red', data = effects_i, shape = 4, size=4) +
  scale_x_discrete("") +
  ggtitle(sprintf('Predicted value for instance: %.0f\nAverage predicted value: %.0f\nActual value: %.0f', pred_i, predictions_mean, y[i]))



# If we average the predictions for the training data instances, we get an average of 4504.
# In comparison, the prediction of the 6-th instance is small, since only 1571 bicycle rents are predicted.
# The effect plot reveals the reason why. 
# The boxplots show the distributions of the effects for all instances of the dataset, the crosses show the effects for the 6-th instance.
# The 6-th instance has a low temperature effect because on this day the temperature was 2 degrees, which is low compared to most other days (and remember that the weight of the temperature feature is positive).
# Also, the effect of the trend feature “days_since_2011” is small compared to the other data instances because this instance is
# from early 2011 (5 days) and the trend feature also has a positive weight.




# -->
# Judging by the attributes that constitute a good explanation, linear models do not create the best explanations. 
# They are contrastive, but the reference instance is a data point where all numerical features are zero and the categorical features are at their reference categories.
# This is usually an artificial, meaningless instance that is unlikely to occur in your data or reality.
# There is an exception: If all numerical features are mean centered (feature minus mean of feature) and all categorical features are effect coded, 
# the reference instance is the data point where all the features take on the mean feature value.
# This might also be a non-existent data point, but it might at least be more likely or more meaningful.

# In this case, the weights times the feature values (feature effects) explain the contribution to the predicted outcome contrastive to the “mean-instance”.

# Another aspect of a good explanation is selectivity, which can be achieved in linear models by using less features or by training sparse linear models.
# But by default, linear models do not create selective explanations.
# Linear models create truthful explanations, as long as the linear equation is an appropriate model for the relationship between features and outcome.
# The more non-linearities and interactions there are, the less accurate the linear model will be and the less truthful the explanations become.
# Linearity makes the explanations more general and simpler. 
