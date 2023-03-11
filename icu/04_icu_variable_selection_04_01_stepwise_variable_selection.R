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
icu.full <- glm(died ~ ., data = ICU2, family = binomial)

icu.full1 <- update(icu.full, . ~ . - renal - fracture)




# ------------------------------------------------------------------------------
# Stepwise variable selection
# ------------------------------------------------------------------------------

library(MASS)

icu.step1 <- stepAIC(icu.full1, trace = TRUE, direction = "both")

icu.step1$anova



# ---------
# Alternatively we can use the BIC criterion, which generally will select a smaller model when the sample size is reasonably large
icu.step2 <- stepAIC(icu.full1, trace = TRUE, direction = "both", k = log(nrow(ICU2)))

icu.step2$anova



# -->
# This model differs from model icu.step1 selected using AIC in the last 3 steps, which also removed ph, pco, and systolic



