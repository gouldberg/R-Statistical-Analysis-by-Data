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



# ----------
HSB$sector <- factor(HSB$sector, levels = c("Public", "Catholic"))

hsb.lme.1 <- lme(mathach ~ mean.ses * cses + sector * cses,
                 random = ~ cses | school, data = HSB)

hsb.lme.2 <- lme(mathach ~ mean.ses * cses + sector * cses,
                 random = ~ 1 | school, data = HSB)

hsb.lmer.1 <- lmer(mathach ~ mean.ses * cses + sector * cses + (cses | school), data = HSB)

hsb.lmer.2 <- lmer(mathach ~ mean.ses * cses + sector * cses + (1 | school), data = HSB)


mod_obj <- hsb.lme.2



# ------------------------------------------------------------------------------
# Model understanding:  School effect
# ------------------------------------------------------------------------------

# fixed-effects intercept and sector coefficient
fixef(mod_obj)



# the mode of the conditional distribution of each beta given the fitted model as predictions of hte beta
# random intercepts (but not random slopes)
# Because the random intercepts have an expectation of zero, the predicted random effects are deviations from the fixed-effect intercept.
head(ranef(mod_obj))

head(summary(mod_obj)$coefficients$random$school)



# ----------
summary(ranef(mod_obj)[[1]])


# -->
# The difference between the best and the worst is about 7.88



# ----------
# predicted intercept for each school
# representing average mathach in the school adjusted for school mean.ses (but not for sector).
school.effects <- data.frame(ranef(mod_obj)$"(Intercept)")
names(school.effects) <- "Intercept"

school.effects$sector <- MathAchSchool$sector

school.effects$intercept <- school.effects$Intercept + ifelse(school.effects$sector == "Public", fixef(mod_obj)[1], sum(fixef(mod_obj)[c(1,4)]))

head(school.effects)


