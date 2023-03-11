setwd("//media//kswada//MyFiles//R//attenu")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  attenu
# ------------------------------------------------------------------------------

data("attenu")


str(attenu)

dim(attenu)


car::some(attenu)



# ----------
attenu <- na.exclude(attenu)



# ----------
mmod <- lmer(accel^0.25 ~ log(dist) + mag + (1 | station) + (1 | event), data = attenu)



# ------------------------------------------------------------------------------
# Test the random effect terms for significance by parametric bootstrap
# ------------------------------------------------------------------------------

mmod <- lmer(accel^0.25 ~ log(dist) + mag + (1 | station) + (1 | event), data = attenu)

mmodr <- lmer(accel^0.25 ~ log(dist) + mag + (1 | event), data = attenu)

mmodr2 <- lmer(accel^0.25 ~ log(dist) + mag + (1 | station), data = attenu)


confint(mmod, method = "boot")

confint(mmodr, method = "boot")

confint(mmodr2, method = "boot")


# -->
# Note that the confidence interval for log(dist) coefficient is changed not much by removing station but much by event

