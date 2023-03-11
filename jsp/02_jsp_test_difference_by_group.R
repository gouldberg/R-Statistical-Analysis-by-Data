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
# Test difference by group (social) 
# ------------------------------------------------------------------------------

# calculate group mean and variance
by(jspr$math, jspr$social, FUN = mean)
# aggregate(jspr$math, by = list(jspr$social), FUN = "mean")

aggregate(jspr$math, by = list(jspr$social), FUN = "var")



# ----------
# Note that one-way anova does not apply due to unequal sample size
aov(math ~ social, data = jspr)




# ----------
# Kruskal-Wallis rank sum test
kruskal.test(math ~ social, data = jspr)

kruskal.test(raven ~ social, data = jspr)

kruskal.test(english ~ social, data = jspr)


# -->
# rejected the null hypothesis that the medians are not different
