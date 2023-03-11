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
# data exploration:  continuous Y vs. continuous X  by group   by ggplot
# ------------------------------------------------------------------------------

library(ggplot2)

gg <- ggplot(jspr, aes(raven, math)) + 
  stat_smooth(method = "loess", size = 2, fill = "blue", alpha = 0.25) +
  stat_smooth(method = "lm", color = "red", size = 1.25, se = FALSE) +
  labs(y = "math", x = "raven")


gg + facet_wrap(~ school)

gg + facet_wrap(~ gender)




# ------------------------------------------------------------------------------
# data exploration:  continuous Y vs. continuous X  by group   by coplot
# ------------------------------------------------------------------------------

formula = math ~ raven | gender

coplot(formula, data = jspr, ylab = "math", xlab = "raven", las=1)



# ----------
formula = math ~ english | gender

coplot(formula, data = jspr, ylab = "english", xlab = "raven", las=1)




# ------------------------------------------------------------------------------
# data exploration:  continuous Y vs. continuous X  by group   by xyplot
# ------------------------------------------------------------------------------

xyplot(math ~ raven | school, data = jspr, type = c("p", "g", "smooth"))

xyplot(math ~ raven | social, data = jspr, type = c("p", "g", "smooth"))



# ----------
xyplot(math ~ english | social, data = jspr, type = c("p", "g", "smooth"))




# ------------------------------------------------------------------------------
# data exploration:  continuous Y vs. continuous X  by group   by scatterplot
# ------------------------------------------------------------------------------

formula <- math ~ raven | social

scatterplot(formula, data = jspr)



# ----------
formula <- math ~ english | social

scatterplot(formula, data = jspr)

