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
# We shall take as our response the math test score result from the final year and try to model this as a function of gender,
# social class and the Raven's test score from the first year which math be taken as a measure of ability when entering the school.

jspr <- jsp[jsp$year == 2,]


car::some(jspr)




# ------------------------------------------------------------------------------
# data exploration
# ------------------------------------------------------------------------------

summary(jspr)


# -->
# unequal sample size by social



# ----------
library(vcd)

mosaic(xtabs(~ school + social, data = jspr), shade = TRUE, gp_args = list(interpolate = 1:4))



# -->
# this is not much informative ...

