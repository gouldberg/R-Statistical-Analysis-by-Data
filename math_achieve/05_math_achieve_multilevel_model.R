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
# Multilevel model
#   - Within schools, we have the regression of match achievement on individual-level cses.
#     Because cses is centered at zero in each school, the intercept is interpretable at the adjusted mean math achievement in a school,
#     and the slope is interpreted as the average change in math achievement associated with a one-unit increase in SES.
#   - Second, as the school level, and also following Raudenbush and Bryk, as well as Singer, we entertain the possibility that the schools intercepts
#     and slopes depende upon sector and upon the average level of SES in the schools
# ------------------------------------------------------------------------------


HSB$sector <- factor(HSB$sector, levels = c("Public", "Catholic"))


hsb.lme.1 <- lme(mathach ~ mean.ses * cses + sector * cses,
                 random = ~ cses | school, data = HSB)


# summary(hsb.lme.1)
S(hsb.lme.1)



# -->
# The coefficient for the interaction meanses:cses = 1.039, gives the average change in the within-school SES slope associated with a one-unit
# increment in the school's mean SES

# The panel in the output below the fixed effects displays estimates of the variance and covariance parameters for the random effects,
# in the form of standard deviations and correlations.


# ----------
car::Anova(hsb.lme.1)

