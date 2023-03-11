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



# ------------------------------------------------------------------------------
# Remove non-significant variables whose coefficients have signs that don't make sense from the substance of th eproblem
# ------------------------------------------------------------------------------

# in the full model, both renal (history of chronic renal failure) and infect (infection probable at ICU admission) have negative signs,
# meaning that their presense decreases the odds of death

icu.full1 <- update(icu.full, . ~ . - renal - fracture)



# ----------
# as expected they make little difference
anova(icu.full1, icu.full, test = "Chisq")



