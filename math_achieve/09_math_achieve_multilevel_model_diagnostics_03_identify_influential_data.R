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

hsb.lmer.1 <- lmer(mathach ~ mean.ses * cses + sector * cses + (cses | school), data = HSB)


# mod_obj <- hsb.lme.1
mod_obj <- hsb.lmer.1



# ------------------------------------------------------------------------------
# The collection of index plot
#   - for mixed-effects models, influence() refits the model deleting each of group in turn,
#     and provide deletion statistics
#   - influenceIndexPlot() create plots for both the fixed effects, displaying influence on the coefficients and Cook's distances,
#     and the random-effect parameters
# ------------------------------------------------------------------------------

# IT TAKES TIME !!!:  2 min
infl <- influence(mod_obj, groups = c("school"))

infl



# ----------
# car::incluenceIndexPlot can be applied to lmer object

car::influenceIndexPlot(mod_obj, vars = c("Cook", "studentized", "hat"), id.n = 4)

car::influenceIndexPlot(mod_obj, var = "var.cov.comps")

# help("influence.mixed.models")

