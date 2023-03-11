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
# Test random effects for "cses" slope by exactRLRT()
# ------------------------------------------------------------------------------

HSB$sector <- factor(HSB$sector, levels = c("Public", "Catholic"))


hsb.lmer.2 <- lmer(mathach ~ mean.ses * cses + sector * cses + (cses | school), data = HSB)


hsb.lmer.4 <- lmer(mathach ~ mean.ses * cses + sector * cses + (1 | school), data = HSB)



# ----------
# we can obtain p-value also by exactRLRT()
# m: fitted model under the alternative or, for testing in models with multiple variance components, the reduced model containing only the random effect to be tested.
# mA: the full model under the alternative for testing in models with multiple variance components
# m0: the model under the null for testing in models with multiple variance components

library(RLRsim)

exactRLRT(m = hsb.lmer.4)

exactRLRT(mA = hsb.lmer.2, mA = hsb.lmer.2, m0 = hsb.lmer.2)

