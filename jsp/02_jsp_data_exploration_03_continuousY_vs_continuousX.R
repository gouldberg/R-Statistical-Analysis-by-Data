setwd("//media//kswada//MyFiles//R//jsp")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  jsp
# ------------------------------------------------------------------------------

data("jsp", package = "faraway")

str(jsp)

car::some(jsp)



# ----------
# We shall take as our response he math test score result from the final year and try to model this as a function of gender,
# social class and the Raven's test score from the first year which math be taken as a measure of ability when entering the school.

jspr <- jsp[jsp$year == 2,]


car::some(jspr)



# ------------------------------------------------------------------------------
# data exploration:  continuous Y vs. continuous X
# ------------------------------------------------------------------------------

plot(jitter(math) ~ jitter(raven), data = jspr, ylab = "math", xlab = "raven", cex.lab = 1.25, pch = 20, col = gray(0.7))
lines(lowess(jspr$raven, jspr$math), col = "blue", lwd = 1)



# -->
# We can see the positive correlation between the Raven's test score and the final math score.
# The Maximum math score was 40, which reduces the variability at the upper end of the scale.



# ------------------------------------------------------------------------------
# data exploration:  continuous Y vs. continuous X   by ggplot
# ------------------------------------------------------------------------------

library(ggplot2)

ggplot(jspr, aes(x = raven, y = math)) + xlab("Raven Score") + ylab("Math Score") + geom_point(position = position_jitter(), alpha = 0.3)




# ------------------------------------------------------------------------------
# data exploration:  continuous Y vs. continuous X   by scatterplot
# ------------------------------------------------------------------------------

formula <- math ~ raven

car::scatterplot(formula, data = jspr)

