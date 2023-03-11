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
# Logistic regression model with all variables included
# ------------------------------------------------------------------------------

icu.full <- glm(died ~ ., data = ICU2, family = binomial)


# ----------
# lmtest::coeftest() is simple version of summary()

summary(icu.full)

lmtest::coeftest(icu.full)




# ------------------------------------------------------------------------------
# Simultaneous global test
# H0: beta = 0 that all regression coefs are zero
#
#  --> If this test is not significant, it makes little sense to use selection methods to choose individually significant predictors.
# ------------------------------------------------------------------------------

# simultaneous global test of H0: beta = 0 that all regression coefs are zero
LRtest <- function(model) c(LRchisq = (model$null.deviance - model$deviance), df = (model$df.null - model$df.residual))



# ----------
( LR <- LRtest(icu.full) )

( pvalue <- 1 - pchisq(LR[1], LR[2]) )



# -->
# Simultaneous global test is significant, it makes sense to use selection methods to choose individually significant predictors


