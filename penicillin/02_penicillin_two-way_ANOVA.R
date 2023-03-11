setwd("//media//kswada//MyFiles//R//penicillin")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  penicillin
# ------------------------------------------------------------------------------
data("penicillin", package = "faraway")

str(penicillin)

car::some(penicillin)



# ------------------------------------------------------------------------------
# 2-way ANOVA
# ------------------------------------------------------------------------------

# We have specified sum contrasts here instead of the default treatment contrasts
# to make the later connection to the corresponding random effects clearer.
op <- options(contrasts = c("contr.sum", "contr.poly"))


lmod <- aov(yield ~ blend + treat, data = penicillin)

summary(lmod)

options(op)


# -->
# We see that there is no significant difference between the treatments, but there is between the blends.



# ----------
# We need coef() to extract the coefficient for aov() model
coef(lmod)




# ----------
# model.tables() computes summary tables for model fits, especially complex aov fits
model.tables(lmod, type = "means")

model.tables(lmod, type = "effects", se = TRUE)




# ------------------------------------------------------------------------------
# ANOVA estimators and
# intraclass correlation coefficient
# ------------------------------------------------------------------------------
# For random effects model, 
# we can compute the variance of the operator effects

# number of samples for each operator (this is balanced data)
n <- 5
# number of levels
a <- 4

summary(lmod)


SSE <- 1.70
SSA <- 1.34

( MSE <- SSE / (a * (n - 1)) )
( MSA <- SSA / (a - 1) )

( sigma2_alpha <- (MSA - MSE) / n )



# ----------
# intraclass correlation coefficient
sigma2_alpha / (sigma2_alpha + MSE)




