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
# Test fixed effects by using adjusted degrees of freedom (Kenward-Roger adjusted F-test)
#  - Likelihood-ratio-type test for fixed effects in mixed models fit by REML are generally inappropriate.
#    One approach to obtaining more accurate inferences in LMMs fit by REML is to adjust the estiamted covariance matrix
#  - of the fixed effects to reduce the typically downward bias of the coefficient
#    standard errors. as proposed by Kenward and Roger (1997),
#    and to adjust degrees of freedom for Wald t- and F-tests, applying a method introduced by Satterthwaite.
# ------------------------------------------------------------------------------


HSB$sector <- factor(HSB$sector, levels = c("Public", "Catholic"))



# ----------
library(nlme)

hsb.lme.2 <- lme(mathach ~ mean.ses * cses + sector * cses,
                 random = ~ 1 | school, data = HSB)


# Anova for lme model does not produce Wald F Tests with Kenward-Roger df
Anova(hsb.lme.2, test = "F")



# ----------
library(lme4)

hsb.lmer.2 <- lmer(mathach ~ mean.ses * cses + sector * cses + (1 | school), data = HSB)


# IT TAKES TIME !!!:  5 min
# Wald F tests with Kenward-Roger adjusted df
Anova(hsb.lmer.2, test = "F")

Anova(hsb.lmer.2)



# -->
# In this case, with many schools and a moderate number of students within each school,
# the Kenward-Roger tests are essentially the same as Wald chi-square tests using the naively computed covariance matrix
# for the fixed effects.



# ------------------------------------------------------------------------------
# Test fixed effects by using adjusted degrees of freedom (Kenward-Roger adjusted F-test)
# - The pbkrtest package implements the Kenward-Roger method
# ------------------------------------------------------------------------------


library(pbkrtest)


# with mean.ses * cses
hsb.lmer.2 <- lmer(mathach ~ mean.ses * cses + sector * cses + (1 | school), data = HSB)


# without mean.ses * cses
hsb.lmer.3 <- lmer(mathach ~ sector * cses + (1 | school), data = HSB)



# ----------
# IT TAKES TIME !!!  1 min
KRmodcomp(hsb.lmer.2, hsb.lmer.3)


