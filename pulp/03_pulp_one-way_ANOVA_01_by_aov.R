setwd("//media//kswada//MyFiles//R//pulp")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  pulp
# ------------------------------------------------------------------------------

data("pulp", package = "faraway")

str(pulp)

car::some(pulp)



# ------------------------------------------------------------------------------
# Fixed effects one-way ANOVA
#   - y(ij) = mu + alpha(i) + e(ij) 
# ------------------------------------------------------------------------------

# We have specified sum contrasts here instead of the default treatment contrasts
# to make the later connection to the corresponding random effects clearer.
op <- options(contrasts = c("contr.sum", "contr.poly"))



# ----------
lmod <- aov(bright ~ operator, data = pulp)

summary(lmod)



# ----------
# We need coef() to extract the coefficient for aov() model
coef(lmod)

options(op)



# -->
# We see that the operator effect is significant with a p-value of 0.023.
# The estimate of sigma^2 is 0.106 and the estimated overall mean is 60.40.

# For sum contrasts (sum(alpha(i)) = 0), so we can calculate the effect for the fourth operator as 0.16 + 0.34 - 0.22 = 0.28.



# ------------------------------------------------------------------------------
# Variance of the operator effect 
# ------------------------------------------------------------------------------

( 0.447 - 0.106 ) / 5




# ------------------------------------------------------------------------------
# model.tables:  computes summary tables for model fits, espeically complex aov fits
# ------------------------------------------------------------------------------

model.tables(lmod, type = "means")

model.tables(lmod, type = "effects", se = TRUE)



# ----------
plot(effects::predictorEffects(lmod))


