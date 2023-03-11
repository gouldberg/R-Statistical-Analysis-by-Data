setwd("//media//kswada//MyFiles//R//reelection")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Reelection
# ------------------------------------------------------------------------------

data("Reelection", package = "pder")


str(Reelection)


dim(Reelection)


car::some(Reelection)



# ------------------------------------------------------------------------------
# fit logit and probit models by glm
# ------------------------------------------------------------------------------


elect.l <- glm(reelect ~ ddefterm + ddefey + gdppc + dev + nd + maj,
               data = Reelection,
               family = "binomial", subset = narrow)


elect.p <- update(elect.l, family = binomial(link = "probit"))




summary(elect.l)


summary(elect.p)




# ----------
library(textreg)


screenreg(list(logit = elect.l, probit = elect.p), digits = 3)




# ------------------------------------------------------------------------------
# Term significance
# ------------------------------------------------------------------------------


# Wald Test:  estimated coefficients (Wald test)
lmtest::coeftest(elect.l)

lmtest::coeftest(elect.p)



# ----------
# Term significance:  Type II tests of each effect

car::Anova(elect.l)

car::Anova(elect.p)




# ------------------------------------------------------------------------------
# Model comparison (goodness of fit):  Likelihood ratio test
# ------------------------------------------------------------------------------

# Two models are nested. Likelihood ratio test can be applied

anova(elect.l, elect.p, test="Chisq")


# -->
# not much difference


# ----------
# Likelihood ratio tests of goodness of fit, together with AIC and BIC statistics
vcdExtra::LRstats(elect.l, elect.p)



# ------------------------------------------------------------------------------
# Goodness of Fit:  The proportion of deviance explained  (Nagelkerke)
# ------------------------------------------------------------------------------

# Nagelkerke R^2
n <- elect.l$df.null + 1

(1 - exp((elect.l$dev - elect.l$null)/n)) / (1 - exp(-elect.l$null/n))



# ----------
# McFadden's peudo R^2
1 - elect.l$deviance / elect.l$null.deviance




# ------------------------------------------------------------------------------
# Goodness of Fit:  Hosmer - Lemeshow Test
# ------------------------------------------------------------------------------

library(ResourceSelection)



# g = 10:  number of bins to use to calcualte quantiles
( hl <- hoslem.test(elect.l$y, fitted(elect.l), g = 10) )


# -->
# Hosmer - Lemeshow Test:  p = 0.8104 indicates some evidence of good fit  ...



# Observed vs Expected
cbind(hl$observed, hl$expected)



