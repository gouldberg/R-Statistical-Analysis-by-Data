setwd("//media//kswada//MyFiles//R//icu")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Death in the ICU
# ------------------------------------------------------------------------------

data("ICU", package = "vcdExtra")

str(ICU)

dim(ICU)


car::some(ICU)



# ----------
# Here we do not apply "rage" and "coma"
# race, coma is represented initially as 3-level factors, but the recoded to binary variables (white, uncons)
# we apply binary variables

ICU2 <- ICU[, -c(4, 20)]

str(ICU2)



# ----------
icu.step2 <- glm(died ~ age + cancer + admit + uncons, data = ICU2, family = binomial)

mod_obj <- icu.step2



# ------------------------------------------------------------------------------
# Adding quadratic or cubic terms by poly
# ------------------------------------------------------------------------------

icu.step2a <- glm(died ~ poly(age - mean(age), 2) + cancer + admit + uncons, data = ICU2, family = binomial)

icu.step2b <- glm(died ~ poly(age - mean(age), 3) + cancer + admit + uncons, data = ICU2, family = binomial)


summary(icu.step2a)

summary(icu.step2b)



# -->
# quardratic and cubic terms are NOT significant



# ----------
anova(icu.step2, icu.step2a, icu.step2b, test = "Chisq")


