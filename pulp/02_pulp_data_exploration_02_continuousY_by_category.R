setwd("//media//kswada//MyFiles//R//pulp")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  pulp
# ------------------------------------------------------------------------------

data("pulp", package = "faraway")

str(pulp)

car::some(pulp)



# ------------------------------------------------------------------------------
# Data exploration:  continuous Y by category  by densityPlot
# ------------------------------------------------------------------------------

densityPlot(pulp$bright, g = pulp$operator)



# ------------------------------------------------------------------------------
# Data exploration:  continuous Y by category  by boxplot
# ------------------------------------------------------------------------------

boxplot(bright ~ operator, data = pulp, varwidth = TRUE)



# ------------------------------------------------------------------------------
# Data exploration:  continuous Y by category  by ggplot
# ------------------------------------------------------------------------------

library(ggplot2)

ggplot(pulp, aes(x = operator, y = bright)) + geom_point(position = position_jitter(width = 0.1, height = 0.0))



# ------------------------------------------------------------------------------
# Data exploration:  group mean and variance
# ------------------------------------------------------------------------------

aggregate(pulp$bright, by = list(pulp$operator), FUN = "mean")

aggregate(pulp$bright, by = list(pulp$operator), FUN = "var")



# -->
# Note that this is balanced sample size for each operator,
# but the variance is different.

# indicating that simple ANOVA or MLE estimates of between subject (operator) variance may be biased.

