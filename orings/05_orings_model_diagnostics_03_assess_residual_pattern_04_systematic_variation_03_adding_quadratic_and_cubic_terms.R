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
# Adding quadratic or cubic terms by poly
# ------------------------------------------------------------------------------

lmod2a <- glm(cbind(damage, 6 - damage) ~ poly(temp, 2), data = orings, family = quasibinomial)

lmod2b <- glm(cbind(damage, 6 - damage) ~ poly(temp, 3), data = orings, family = quasibinomial)


summary(lmod2a)

summary(lmod2b)



# -->
# quardratic and cubic terms are not significant !!



# ----------
anova(qlmod, qlmod2a, qlmod2b, test = "Chisq")



# ----------
# Nagelkerke R^2
n <- qlmod$df.null + 1
(1 - exp((qlmod$dev - qlmod$null)/n)) / (1 - exp(-qlmod$null/n))


n <- qlmod2a$df.null + 1
(1 - exp((qlmod2a$dev - qlmod2a$null)/n)) / (1 - exp(-qlmod2a$null/n))


n <- qlmod2b$df.null + 1
(1 - exp((qlmod2b$dev - qlmod2b$null)/n)) / (1 - exp(-qlmod2b$null/n))



# -->
# The model with cubic terms are the best !!



# ------------------------------------------------------------------------------
# Goodness of Fit:  Hosmer - Lemeshow Test
# ------------------------------------------------------------------------------

library(ResourceSelection)



# g = 10:  number of bins to use to calcualte quantiles
( hl <- hoslem.test(qlmod2b$y, fitted(qlmod2b), g = 10) )


# -->
# Hosmer - Lemeshow Test:  p = 1 indicates good fit


# Observed vs Expected
cbind(hl$observed, hl$expected)

