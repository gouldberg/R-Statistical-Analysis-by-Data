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

mmod2 <- lmer(math ~ raven * social + (1 | school) + (1 | school : class), data = jspr)

glin <- lm(math ~ raven * social * gender, data = jspr)

glin2 <- lm(math ~ raven * social, data = jspr)





# ------------------------------------------------------------------------------
# Compare fixed effects coefficients
# ------------------------------------------------------------------------------


# glin vs. mmod
data.frame(lin = round(coef(glin), 3), multilevel = round(fixef(mmod), 3))



car::Anova(glin)

car::Anova(mmod)




# ----------
# glin2 vs. mmod2
data.frame(lin = round(coef(glin2), 3), multilevel = round(fixef(mmod2), 3))



car::Anova(glin2)

car::Anova(mmod2)




# -->
# in glin2 and mmod2, the coefs for social 4 and 6 are negative
# but
# in glin and mmod1, the coefs for social 4 and 6 are positive