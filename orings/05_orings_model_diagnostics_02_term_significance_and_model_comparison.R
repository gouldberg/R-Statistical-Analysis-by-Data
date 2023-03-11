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
# Model comparison of two models:  Likelihood Ratio Test
# ------------------------------------------------------------------------------

# Deviance の差での検定：　推定モデルが切片モデルより適合しているか？
pchisq(lmod$null.deviance - lmod$deviance, lmod$df.null - lmod$df.residual, lower = FALSE)


# --> 
# We conclude tha effect of launch temperature is statistically significant
# 棄却されるので、切片モデルが推定モデルよりも適合している、とは言えない



# ----------
# single term deletion
drop1(lmod, test="Chi")



# ----------
# Terms added sequentially
# Test whether temperature significantly improves prediction of failuer probability
anova(lmod, test = "Chisq")
