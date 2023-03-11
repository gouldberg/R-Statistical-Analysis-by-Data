setwd("//media//kswada//MyFiles//R//psid")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  psid
# ------------------------------------------------------------------------------
data("psid", package = "faraway")

str(psid)

car::some(psid)



# ------------------------------------------------------------------------------
# Test fixed effects by using adjusted degrees of freedom (Kenward-Roger adjusted F-test)
#  - More reliable F-tests can be achieved by using adjusted degrees of freedom.
#    The pbkrtest package implements the Kenward-Roger method
#  - This method can be generalized to a much wider class of problems.
# ------------------------------------------------------------------------------

library(pbkrtest)


mmod <- lmer(log(income) ~ cyear * sex + age + educ + (cyear | person), data = psid, REML = FALSE)


mmodr <- lmer(log(income) ~ cyear + sex + age + educ + (cyear | person), data = psid, REML = FALSE)



# We use the Kenward-Roger adjusted F-test
KRmodcomp(mmod, mmodr)



# -->
# cyear * sex interaction term is marginally significant so there is no justification to simplify the model by removing this term.
# Female incomes are incresing faster than male incomes.
