# setwd("//media//kswada//MyFiles//R//owls")
setwd("//media//kswada//MyFiles//R//Mixed_effects_and_multilevel_model//owls")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Owls
# ------------------------------------------------------------------------------

Owls <- read.table(file = "Owls.txt", header = TRUE)


str(Owls)

dim(Owls)

car::some(Owls)



# ------------------------------------------------------------------------------
# The Random Intercept and Slope Model by lme (linear mixed effects model)
# ------------------------------------------------------------------------------

Form <- formula(LogNeg ~ SexParent * FoodTreatment + SexParent * ArrivalTime)


M2.lme <- lme(Form, random = ~ 1 + ArrivalTime | Nest, method = "REML", data = Owls)


summary(M2.lme)



# ----------
anova(M.gls, M1.lme, M2.lme)



# ----------
AIC(M.gls, M1.lme, M2.lme)


# -->
# Note that the random intercept and slope model has a lower AIC than the earlier models.



# -->
# The amount of variation around the population intercept is 0.8667^2 = 0.751
# The estimated variation in slopes for ArrivalTime is 0.03378^2 = 0.00114,
# showing that there is considerably more variation in intercepts than in slopes at ArrivalTime.
# There is a correlation between the random intercepts and slopps. Its value of -0.996 is rather high (causing potential numerical problems),
# but indicates that Nest with a high positive intercept also have a high negative slope.
