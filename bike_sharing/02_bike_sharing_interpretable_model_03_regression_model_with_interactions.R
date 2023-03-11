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
# Include interaction between the temperature and the working day
# ------------------------------------------------------------------------------
bike.features.of.interest <- c('season','holiday', 'workingday', 'weathersit', 'temp', 'hum', 'windspeed', 'days_since_2011')

X = bike[bike.features.of.interest]

y = bike[,'cnt']

dat = cbind(X, y)



# ----------
# This time, we additionally consider an interaction between the temperature and the working day feature. 
mod = lm(y ~ . + temp * workingday, data = dat, x = TRUE)

summary(mod)


lm_summary = summary(mod)$coefficients

lm_summary_print = lm_summary

rownames(lm_summary_print) = pretty_rownames(rownames(lm_summary_print))

# var name becomes to long otherwise
rownames(lm_summary_print)[rownames(lm_summary_print) == "weathersitRAIN/SNOW/STORM"] = "weathersitRAIN/..."

kable(cbind(lm_summary_print[,c('Estimate', 'Std. Error')], confint(mod)), digits = 1, col.names = c('Weight', 'Std. Error', "2.5%","97.5%"))



# -->
# The additional interaction effect is negative (-21.8) and differs significantly from zero, as shown by the 95% confidence interval, which does not include zero.
# By the way, the data are not iid, because days that are close to each other are not independent from each other.
# Confidence intervals might be misleading, just take it with a grain of salt. 
# The interaction term changes the interpretation of the weights of the involved features.
# Does the temperature have a negative effect given it is a working day ? The answer is no, even if the table suggests it to an untrained user.

# We cannot interpret the “workingdayWORKING DAY:temp” interaction weight in isolation, 
# since the interpretation would be: “While leaving all other feature values unchanged, increasing the interaction effect of temperature for working day decreases the predicted number of bikes.”
# But the interaction effect only adds to the main effect of the temperature.
# Suppose it is a working day and we want to know what would happen if the temperature were 1 degree warmer today.
# Then we need to sum both the weights for “temp” and “workingdayWORKING DAY:temp” to determine how much the estimate increases.



# ------------------------------------------------------------------------------
# Interaction plot
# ------------------------------------------------------------------------------
# It is easier to understand the interaction visually.
# By introducing an interaction term between a categorical and a numerical feature, we get two slopes for the temperature instead of one.
# The temperature slope for days on which people do not have to work (‘NO WORKING DAY’) can be read directly from the table (125.4). 
# The temperature slope for days on which people have to work (‘WORKING DAY’) is the sum of both temperature weights (125.4 -21.8 = 103.6). 
# The intercept of the ‘NO WORKING DAY’-line at temperature = 0 is determined by the intercept term of the linear model (2185.8).
# The intercept of the ‘WORKING DAY’-line at temperature = 0 is determined by the intercept term + the effect of working day (2185.8 + 451.9 = 2637.7).

# The effect (including interaction) of temperature and working day on the predicted number of bikes for a linear model. 
# Effectively, we get two slopes for the temperature, one for each category of the working day feature.

interactions::interact_plot(mod, pred = "temp", modx = "workingday")



