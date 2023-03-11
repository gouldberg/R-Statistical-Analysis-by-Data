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



# ----------
library(lme4)

mmod <- lmer(math ~ raven * social * gender + (1 | school) + (1 | school : class), data = jspr)

jspr$craven <- jspr$raven - mean(jspr$raven)
mmod_final <- lmer(math ~ craven * social + (1 | school) + (1 | school : class), data = jspr)



# ----------
mod_obj <- mmod



# ------------------------------------------------------------------------------
# Model understanding:  School effect
# ------------------------------------------------------------------------------

# These represent a ranking of the schools adjusted for the quality of the intake and the social class of the students.
adjscores <- ranef(mod_obj)$school[[1]]

summary(adjscores)


# -->
# The difference between the best and the worst is about five points on the math test.



# ----------
# compare this with an unadjusted ranking that simply takes the average score achieved by the school, centered by the overall average.

rawscores <- coef(lm(math ~ school - 1, jspr))

rawscores <- rawscores - mean(rawscores)

plot(rawscores, adjscores)
sint <- c(9, 14, 29)
text(rawscores[sint], adjscores[sint] + 0.2, c("9", "15", "30"))



# -->
# School 10 is listed but has no students, hence the need to adjust the labeling.

# School 15: looks best on the raw scores but after adjustment, it drops to 15th place.
# This is a school that apparently performs well, but when the quality of the incoming students is considered, its performance is not so impressive.

# School 30: This school looks average on the raw scores, but is doing quite well given the ability of the incoming students.

# School 9: actually doing a poor job despite raw scores that look quite good.

