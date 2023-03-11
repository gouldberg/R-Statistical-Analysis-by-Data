setwd("//media//kswada//MyFiles//R//rmotivation")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Rmotivation
# ------------------------------------------------------------------------------

data("Rmotivation", package = "MPsychoR")


str(Rmotivation)



# ----------
# Here we focus on hybrid motivation only and selct 19 dichotomous items associated with this latent variable

( ind <- grep("hyb", colnames(Rmotivation)) )

HybMotivation <- na.omit(Rmotivation[, ind])

car::some(HybMotivation)



# ------------------------------------------------------------------------------
# Generalizability Theory:  fixed-effects ANOVA, one-facet G-theory
#   - The classical true score model (X = T + E) has only one source of error E.
#     Cronbach extended the reliability concept by combning the true score model with ANOVA techniques in order to account for multiple sources of measurement errors.
#     This framwork is called generalizability theory (G-theory).
#   - In G-theory slang, these multiple error sources are called facets:  items, raters, measurement occasions, etc.
# ------------------------------------------------------------------------------


# ----------
# First demonstrate computing approximation of Cronbach's alpha by ANOVA strategy
library(reshape2)

Hyb1 <- data.frame(HybMotivation, person = 1:nrow(HybMotivation))

car::some(Hyb1)


Hyblong <- melt(Hyb1, id.vars = c("person"), variable.name = "item")

car::some(Hyblong)


Hyblong$person <- as.factor(Hyblong$person)


# fixed-effects ANOVA
summary(aov(value ~ person + item, data = Hyblong))


# Appoximation of Cronbach's alpha can be obtained via the person mean squares and redisual mean squares
round((0.85 - 0.15) / 0.85, 2)



# ----------
# Such an ANOVA strategy is also pursued when computing the intraclass correlation coefficient (ICC), often applied in the medical area
# to determine the reliability of ratings (e.g., participants being rated by judges)

# Here items play the role of raters

icchyb <- psych::ICC(HybMotivation)

icchyb


# -->
# ICC(3, k) = 0.82 is the average fixed raters.
# We see that one-facet example, the ICC is the same as Cronbach's alpha.



# ------------------------------------------------------------------------------
# Generalizability Theory:  random-effects ANOVA, one-facet G-theory
# ------------------------------------------------------------------------------

summary(aov(value ~ person + item, data = Hyblong))


# variance components expressed as standard deviations (sigma of person)  --. 0.191943
sqrt(( 0.85 - 0.15) / 19)


# variance components expressed as standard deviations (sigma of item)  --> 0.2020806
sqrt(( 31.88 - 0.15 ) / 777)




# ------------------------------------------------------------------------------
# Generalizability Theory:  random-effects ANOVA, one-facet G-theory  --> by lme4 package
# ------------------------------------------------------------------------------

library(lme4)

mod <- lmer(value ~ (1 | person) + (1 | item), data = Hyblong)

summary(mod)



# ----------
VarCorr(mod)



# ------------------------------------------------------------------------------
# Generalizability Theory:  compute generalizability coefficient
#   - this is a reliability coefficient which, in one-facet example, is equivalent to Cronbach's alpha
#   - gtheory package performs the lmr4 call internally and based on these results, it computes the generalizability coefficient
# ------------------------------------------------------------------------------

library(gtheory)

gfit <- gstudy(data = Hyblong, formula = value ~ (1 | person) + (1 | item))

dfit <- dstudy(gfit, colname.objects = "person", colname.scores = "value", data = Hyblong)

round(dfit$generalizability, 3)


# -->
# Here one-facet example, generalizability coefficient is equal to Cronbach's alpha








