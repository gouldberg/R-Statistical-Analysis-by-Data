setwd("//media//kswada//MyFiles//R//mammalsleep")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  mammalsleep
# ------------------------------------------------------------------------------

data("mammalsleep", package = "faraway")


str(mammalsleep)


mammalsleep$pdr <- with(mammalsleep, dream / sleep)


head(mammalsleep)



# ----------
qlmod <- glm(pdr ~ log(body) + log(lifespan) + danger, family = quasibinomial, data = mammalsleep)




# ------------------------------------------------------------------------------
# Adding quadratic or cubic terms by poly
# ------------------------------------------------------------------------------

qlmod2a <- glm(pdr ~ poly(log(body),2) + log(lifespan) + danger, data = mammalsleep, family = quasibinomial)

qlmod2b <- glm(pdr ~ poly(log(body),3) + log(lifespan) + danger, data = mammalsleep, family = quasibinomial)


summary(qlmod2a)

summary(qlmod2b)



# -->
# quardratic and cubic terms ARE significant !!



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

