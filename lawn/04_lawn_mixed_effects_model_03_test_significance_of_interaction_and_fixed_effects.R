setwd("//media//kswada//MyFiles//R//lawn")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  lawn
# ------------------------------------------------------------------------------

data("lawn", package = "faraway")

str(lawn)


car::some(lawn)



# ------------------------------------------------------------------------------
# Test the significance of interaction term
# ------------------------------------------------------------------------------

library(pbkrtest)


cmod_f <- lmer(time ~ 1 + manufact * speed + (1 | machine), data = lawn, REML = FALSE)


cmodn_f <- lmer(time ~ 1 + manufact + speed + (1 | machine), data = lawn, REML = FALSE)




# ----------
KRmodcomp(cmod_f, cmodn_f)



# -->
# No significant for interaction term



# ------------------------------------------------------------------------------
# Test the significance of main effects
# ------------------------------------------------------------------------------

library(pbkrtest)


cmodn_f <- lmer(time ~ 1 + manufact + speed + (1 | machine), data = lawn, REML = FALSE)


cmodn2_f <- lmer(time ~ 1 + manufact + (1 | machine), data = lawn, REML = FALSE)


cmodn3_f <- lmer(time ~ 1 + speed + (1 | machine), data = lawn, REML = FALSE)



# ----------
KRmodcomp(cmodn_f, cmodn2_f)


KRmodcomp(cmodn_f, cmodn3_f)



# -->
# Speed effect is significant, but manufact effect is not sifnificant (but marginally)





