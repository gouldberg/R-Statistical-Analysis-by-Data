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


mod_obj <- hsb.lme.1



# ------------------------------------------------------------------------------
# Predicttion
# ------------------------------------------------------------------------------

# predicted coefficients
fixef(mod_obj)
ranef(mod_obj)



# ----------
# We specify the random effects part of the prediction as ~0 meaning that this term is not present.
predict(mod_obj, re.form = ~ 0)

predict(mod_obj)


