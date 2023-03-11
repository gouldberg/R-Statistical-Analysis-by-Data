setwd("//media//kswada//MyFiles//R//orings")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  orings
# ------------------------------------------------------------------------------

data("orings", package = "faraway")

str(orings)

dim(orings)


car::some(orings)



# ----------
lmod <- glm(cbind(damage, 6 - damage) ~ temp, family = binomial, orings)

summary(lmod)



# ------------------------------------------------------------------------------
# Goodness of Fit:  Test whether the model is an adequate fit (deviance base)
# ------------------------------------------------------------------------------

# Deviance のカイ二乗検定
pchisq(deviance(lmod), df.residual(lmod), lower = FALSE)


# -->
# Since this p-value is well in excess of 0.05, we conclude that this model fits sufficiently well.
# But the X^2 distribution is only an approximation that becomes more accurate as the number of trials increase.
# The approximation is very poor when the number of trials <= 5

# 従属変数が二項分布に従い、サンプル数が多い場合は、Devianceは（サンプル数-パラメタ数）の自由度をもつ、カイ二乗分布に従う
# Devianceのカイ二乗検定結果は p=0.7164　-->　棄却されないので、0.05より十分に大きいので、ロジスティック回帰モデルはよく適合しているといえる



# ----------
# Null model fit is inadequate.
# 切片モデルは、棄却されてしまう
pchisq(lmod$null.deviance, lmod$df.null, lower = FALSE)



# ------------------------------------------------------------------------------
# Goodness of Fit:  The proportion of deviance explained  (Nagelkerke)
# ------------------------------------------------------------------------------

# Nagelkerke R^2
n <- lmod$df.null + 1

(1 - exp((lmod$dev - lmod$null)/n)) / (1 - exp(-lmod$null/n))



# ----------
# McFadden's peudo R^2
1 - lmod$deviance / lmod$null.deviance




# ------------------------------------------------------------------------------
# Goodness of Fit:  Hosmer - Lemeshow Test
# ------------------------------------------------------------------------------

library(ResourceSelection)



# g = 10:  number of bins to use to calcualte quantiles
( hl <- hoslem.test(lmod$y, fitted(lmod), g = 5) )



# -->
# Hosmer - Lemeshow Test:  p = 0.8783 indicates good fit



# Observed vs Expected
cbind(hl$observed, hl$expected)


