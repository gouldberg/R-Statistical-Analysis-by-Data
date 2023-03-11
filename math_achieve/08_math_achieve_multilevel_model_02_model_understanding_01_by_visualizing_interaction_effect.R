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
# Model understanding by main effect plot
# ------------------------------------------------------------------------------

library(effects)

plot(predictorEffects(mod_obj))




# ------------------------------------------------------------------------------
# Model understanding by visualizing interaction by sector
# ------------------------------------------------------------------------------


plot(predictorEffects(mod_obj, ~ cses,
                      xlevels = list(mean.ses = round(seq(-1.2, 0.8, length = 6), 1))),
     lines = list(multiline = TRUE, lwd = 4, col = c("black", "darkgray"), lty = c(1,2)),
     confint = list(style = "bands"),
     axes = list(x = list(rug = FALSE)),
     lattice = list(key.args = list(space = "right", columns = 1)))


# -->
# It is clear from the effect plot that the impact of students' SES on math achievement increases as the mean SES in their school rises
# and is greater at fixed levels of mean SES in public schools than in Catholic schools.


