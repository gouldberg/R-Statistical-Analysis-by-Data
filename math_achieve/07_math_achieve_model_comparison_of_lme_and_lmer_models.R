setwd("//media//kswada//MyFiles//R//math_achieve")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  MathAchieve
# ------------------------------------------------------------------------------

data("MathAchieve", package = "nlme")

dim(MathAchieve)

str(MathAchieve)

car::some(MathAchieve)



# ----------
data("MathAchSchool", package = "nlme")

dim(MathAchSchool)

str(MathAchSchool)

car::some(MathAchSchool)



# ----------
names(MathAchieve) <- tolower(names(MathAchieve))
names(MathAchSchool) <- tolower(names(MathAchSchool))


Temp <- MathAchieve %>% group_by(school) %>% summarize(mean.ses = mean(ses))
Temp <- merge(MathAchSchool, Temp, by = "school")
car::brief(Temp)


# ----------
HSB <- merge(Temp[, c("school", "sector", "mean.ses")], MathAchieve[, c("school", "ses", "mathach")], by = "school")


# ----------
HSB$cses <- with(HSB, ses - mean.ses)
car::brief(HSB)



# ------------------------------------------------------------------------------
# Comparison between lme() model and lmer() model
# ------------------------------------------------------------------------------

library(lme4)


hsb.lmer.1 <- lmer(mathach ~ mean.ses * cses + sector * cses + (cses | school), data = HSB)


S(hsb.lmer.1)
S(hsb.lme.1)



# -->
# The estimates of the fixed effects and variance / covariance components are the same as those obtained from lme(),
# but the specification of the model is slightly different: Rather than using a random argument, as in lme(), the random effects in lmer()
# are given directly in the model formula, enclosed in parentheses.

# In most instances, the choice between lme() and lmer() is unimportant,
# but there are some models that can fit with one of these functions but not with the other.
# For example, lme() provides for modeling within-group correlation structures that specify the variance-covariance matrix using only a few parameters,
# a feature not available with lmer().
# On the other hand, lmer() can accomodate crossed (as opposed to nested) random effects, while lme() cannot.

# For example, if we were interested in teacher effects on students' achievement, each student in a high school has several teachers,
# and so students would not be strictly nested within teachers.

# A subtle difference between the S() output for models fit by lme() and lmer() is that
# the former includes p-values and degrees of freedom for Wald t-tests of the estimated coefficients,
# while the latter includes p-values for asymptotic Wald z-tests and no degrees of freedom.



# ------------------------------------------------------------------------------
# model comparison of lmer() models
# ------------------------------------------------------------------------------

# omitting random effect of cses
hsb.lmer.2 <- lmer(mathach ~ mean.ses * cses + sector * cses + (1 | school), data = HSB)


anova(hsb.lmer.1, hsb.lmer.2)
anova(hsb.lme.1, hsb.lme.2)



# -->
# anova() refits the models using ML rather than REML, because LR tests of models fit by REML that differ in their fixed effects are inappropriate.
# In our case, however, the models compared have idential fixed effects and differ only in the random effects.
# A likelihood-ratio test is appropriate even if the models are fit by REML.



# ----------
# We can obtain this test by specifying the argument
# This is the same p-values with model comparison of lme() models
anova(hsb.lmer.1, hsb.lmer.2, refit = FALSE)

anova(hsb.lme.1, hsb.lme.2)




