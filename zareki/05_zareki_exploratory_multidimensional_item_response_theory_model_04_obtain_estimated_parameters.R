setwd("//media//kswada//MyFiles//R//zareki")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  zareki
# ------------------------------------------------------------------------------

data("zareki", package = "MPsychoR")

str(zareki)



# using all binary ZAREKI addition and subtraction items
itzareki <- zareki[, 1:16]




# ------------------------------------------------------------------------------
# person parameters
# ------------------------------------------------------------------------------

# person parameters

head(fscores(zar2d))



# ----------
zareki$F1 <- fscores(zar2d)[,1]


zareki$F2 <- fscores(zar2d)[,2]




# ------------------------------------------------------------------------------
# ANOVA with school year as factor
# ------------------------------------------------------------------------------

summary(aov(F1 ~ class, data = zareki))


summary(aov(F2 ~ class, data = zareki))



# -->
# Significant differences in ability across the 3 years.



# ----------
boxplot(F1 ~ class, data = zareki)


boxplot(F2 ~ class, data = zareki)

