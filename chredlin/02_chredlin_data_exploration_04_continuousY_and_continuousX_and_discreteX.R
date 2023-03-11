setwd("//media//kswada//MyFiles//R//chredlin")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  chredlin
# ------------------------------------------------------------------------------

data("chredlin", package = "faraway")

str(chredlin)




# ------------------------------------------------------------------------------
# Assess relationship between the response variable and each continuous covariate and categorical covariate by ggplot
# ------------------------------------------------------------------------------

library(ggplot2)

ggplot(chredlin, aes(race, involact)) + geom_point() + stat_smooth(method = "lm", col = "blue") + stat_smooth(method = "loess", col = "red")

ggplot(chredlin, aes(fire, involact)) + geom_point() + stat_smooth(method = "lm", col = "blue") + stat_smooth(method = "loess", col = "red")

ggplot(chredlin, aes(theft, involact)) + geom_point() + stat_smooth(method = "lm", col = "blue") + stat_smooth(method = "loess", col = "red")

ggplot(chredlin, aes(age, involact)) + geom_point() + stat_smooth(method = "lm", col = "blue") + stat_smooth(method = "loess", col = "red")

ggplot(chredlin, aes(income, involact)) + geom_point() + stat_smooth(method = "lm", col = "blue") + stat_smooth(method = "loess", col = "red")

ggplot(chredlin, aes(side, involact)) + geom_point(position = position_jitter(width = .2, height = 0))



# -->
# we can see some outlier and influential points.
# We can see that the fitted line sometimes goes below zero which is problematic since observed values of the response cannot be negative.

