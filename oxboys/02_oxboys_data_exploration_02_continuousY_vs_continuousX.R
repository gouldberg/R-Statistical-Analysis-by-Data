setwd("//media//kswada//MyFiles//R//oxboys")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Oxboys
# ------------------------------------------------------------------------------

data("Oxboys", package = "rethinking")

d <- Oxboys

dim(d)

str(d)


car::some(d)



# ------------------------------------------------------------------------------
# data exploration:  continuous Y vs. continuous X
# ------------------------------------------------------------------------------

plot(height ~ age, data = d, ylab = "height", xlab = "age", cex.lab = 1.25, pch = 20, col = gray(0.7))
lines(lowess(d$age, d$height), col = "blue", lwd = 1)



# ------------------------------------------------------------------------------
# data exploration:  continuous Y vs. continuous X  by group   by xyplot
# ------------------------------------------------------------------------------

xyplot(height ~ age, data = d)



# ------------------------------------------------------------------------------
# data exploration:  continuous Y vs. continuous X   by ggplot
# ------------------------------------------------------------------------------

library(ggplot2)

ggplot(d, aes(x = age, y = height)) + xlab("age") + ylab("height") + geom_point(position = position_jitter(), alpha = 0.3) + stat_smooth()



# ------------------------------------------------------------------------------
# data exploration:  continuous Y vs. continuous X  by scatterplot
# ------------------------------------------------------------------------------

formula <- height ~ age

car::scatterplot(formula, data = d)


