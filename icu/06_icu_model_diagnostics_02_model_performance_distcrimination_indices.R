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
library(rms)

dd <- datadist(ICU2);  options(datadist = "dd")

icu.lrm2 <- lrm(died ~ age + cancer + systolic + admit + ph + pco + uncons, data = ICU2)

summary(icu.lrm2)



# ------------------------------------------------------------------------------
# Model Performance:  Discrimination Indices
# ------------------------------------------------------------------------------

head(icu.lrm2, 10)
