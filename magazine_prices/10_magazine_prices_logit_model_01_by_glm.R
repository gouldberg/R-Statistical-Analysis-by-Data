setwd("//media//kswada//MyFiles//R//magazine_prices")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Magazine Prices
# ------------------------------------------------------------------------------

data("MagazinePrices", package = "pder")


str(MagazinePrices)


dim(MagazinePrices)


car::some(MagazinePrices)



# ------------------------------------------------------------------------------
# response variable: "price change" (0 or 1),  by glm
# ------------------------------------------------------------------------------

# included == 1:  year 53 to year 79 (not inlcluding year 40 to 52 and 80)

logitS <- glm(change ~ length + cuminf + cumsales, data = MagazinePrices,
              subset = included == 1, family = binomial(link = "logit"))


logitD <- glm(change ~ length + cuminf + cumsales + magazine, data = MagazinePrices,
              subset = included == 1, family = binomial(link = "logit"))



summary(logitS)


summary(logitD)




# ----------

library(textreg)


screenreg(list(logitS = logitS, logitD = logitD), digits = 3)



# -->
# Note that length effect is negative and cumsales effect is also negative
# and cumsales effect is not significant



# ------------------------------------------------------------------------------
# Term significance
# ------------------------------------------------------------------------------


# Wald Test:  estimated coefficients (Wald test)
lmtest::coeftest(logitS)

lmtest::coeftest(logitD)



# ----------
# Term significance:  Type II tests of each effect

car::Anova(logitS)

car::Anova(logitD)




# ------------------------------------------------------------------------------
# Model comparison (goodness of fit):  Likelihood ratio test
# ------------------------------------------------------------------------------

# Two models are nested. Likelihood ratio test can be applied

anova(logitS, logitD, test="Chisq")


# -->
# model is different



# ----------
# Likelihood ratio tests of goodness of fit, together with AIC and BIC statistics

vcdExtra::LRstats(logitS, logitD)



# ------------------------------------------------------------------------------
# Goodness of Fit:  The proportion of deviance explained  (Nagelkerke)
# ------------------------------------------------------------------------------

# Nagelkerke R^2
n <- logitS$df.null + 1

(1 - exp((logitS$dev - logitS$null)/n)) / (1 - exp(-logitS$null/n))



# logitD is the twice
n <- logitD$df.null + 1

(1 - exp((logitD$dev - logitD$null)/n)) / (1 - exp(-logitD$null/n))



# ----------
# McFadden's peudo R^2
1 - logitS$deviance / logitS$null.deviance




# ------------------------------------------------------------------------------
# Goodness of Fit:  Hosmer - Lemeshow Test
# ------------------------------------------------------------------------------

library(ResourceSelection)



# g = 10:  number of bins to use to calcualte quantiles
( hl <- hoslem.test(logitS$y, fitted(logitS), g = 10) )

( hl <- hoslem.test(logitD$y, fitted(logitD), g = 10) )



# -->
# Hosmer - Lemeshow Test:  p = 0.0526 indicates some evidence of poor fit  ...



# Observed vs Expected
cbind(hl$observed, hl$expected)



