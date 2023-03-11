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



# ----------
library(rms)

dd <- datadist(ICU2);  options(datadist = "dd")

icu.lrm2 <- lrm(died ~ age + cancer + systolic + admit + ph + pco + uncons, data = ICU2)

summary(icu.lrm2)



# ------------------------------------------------------------------------------
# Goodness of Fit:  Test whether the model is an adequate fit (deviance base)
# ------------------------------------------------------------------------------

# Deviance のカイ二乗検定
pchisq(deviance(icu.step1), df.residual(icu.step1), lower = FALSE)



# -->
# Since this p-value is well in excess of 0.05, we conclude that this model fits sufficiently well.
# But the X^2 distribution is only an approximation that becomes more accurate as the number of trials increase.
# The approximation is very poor when the number of trials <= 5

# 従属変数が二項分布に従い、サンプル数が多い場合は、Devianceは（サンプル数-パラメタ数）の自由度をもつ、カイ二乗分布に従う



# ------------------------------------------------------------------------------
# Goodness of Fit:  The proportion of deviance explained  (Nagelkerke)
# ------------------------------------------------------------------------------

# Nagelkerke R^2
n <- icu.step1$df.null + 1

(1 - exp((icu.step1$dev - icu.step1$null)/n)) / (1 - exp(-icu.step1$null/n))



# ----------
# McFadden's peudo R^2
1 - icu.step1$deviance / icu.step1$null.deviance



# ------------------------------------------------------------------------------
# Goodness of Fit:  The proportion of deviance explained  (Nagelkerke)
# ------------------------------------------------------------------------------

# Note that Discrimination Indexes R2 is Nagelkerke R^2
icu.lrm2



# ------------------------------------------------------------------------------
# Goodness of Fit:  Hosmer - Lemeshow Test
# ------------------------------------------------------------------------------

library(ResourceSelection)



# g = 10:  number of bins to use to calcualte quantiles
( hl <- hoslem.test(icu.step1$y, fitted(icu.step1), g = 10) )


# -->
# Hosmer - Lemeshow Test:  p = 0.5131 indicates some efidence of poor fit  ...



# Observed vs Expected
cbind(hl$observed, hl$expected)



# ------------------------------------------------------------------------------
# Squared Pearson Correlation Coefficients of observed outcome with the estimated probability
# ------------------------------------------------------------------------------

pred <- predict(icu.step1, newdata = ICU2, type = "response")

cor((ICU2$died == "Yes")*1, pred)^2

