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
# Fixed effects three-way ANOVA
# ------------------------------------------------------------------------------

# We have spefified sum contrasts here instead of the default treatment contrasts to
# make the later connection to the corresponding random effects clearer
op <- options(contrasts = c("contr.sum", "contr.poly"))



# ----------
# NOTE THAT WE CANNOT DO NESTED ANOVA
lmod <- aov(time ~ manufact + machine + speed, data = lawn)

options(op)


summary(lmod)



# -->
# All terms are significant
# As expected, Mean sq of speed is largest
# Note that Mean Sq of manufact is 3 times larger than machine's.  (Machine's variation is very small)



# ----------
plot(lmod)



# ----------
# We need coef() to extract the coefficient for aov() model
coef(lmod)



# ------------------------------------------------------------------------------
# model.tables:  computes summary tables for model fits, espeically complex aov fits
# ------------------------------------------------------------------------------

model.tables(lmod, type = "means")

model.tables(lmod, type = "effects", se = TRUE)



# ----------
# plot(effects::predictorEffects(lmod))

