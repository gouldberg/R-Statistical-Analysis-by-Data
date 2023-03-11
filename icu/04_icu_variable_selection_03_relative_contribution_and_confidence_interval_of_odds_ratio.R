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
# Relative contribution and confidence interval of odds ratios for each variables
# ------------------------------------------------------------------------------

library(rms)

dd <- datadist(ICU2);  options(datadist = "dd")


icu.lrm1 <- lrm(died ~ ., data = ICU2)


icu.lrm1 <- update(icu.lrm1, . ~ . - renal - fracture)


sum.lrm1 <- summary(icu.lrm1)


sum.lrm1



# ----------
# relative contribution
par(mfrow = c(1,1))

plot(anova(icu.lrm1))



# ----------
# confidence interval of odds ratios (in log scale) for each variables
plot(summary(icu.lrm1), log=TRUE, cex = 1.25, col=rgb(.1, .1, .8, alpha=c(.3, .5, .8)))



exp(coef(mod))



# Odds Ratio:  Wald confidence interval
exp(confint.default(mod, level = 0.95))
summary(modl)
plot(summary(modlrm), log=TRUE, cex = 1.25, col=rgb(.1, .1, .8, alpha=c(.3, .5, .8)))


# Odds Ratio:  Profile likelihod ratio interval
exp(confint(mod, level = 0.95))
