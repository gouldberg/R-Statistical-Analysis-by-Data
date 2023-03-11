setwd("//media//kswada//MyFiles//R//jsp")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  jsp
# ------------------------------------------------------------------------------

data("jsp", package = "faraway")

str(jsp)

car::some(jsp)



# ----------
# We shall take as our response he math test score result from the final year and try to model this as a function of gender,
# social class and the Raven's test score from the first year which math be taken as a measure of ability when entering the school.

jspr <- jsp[jsp$year == 2,]


car::some(jspr)



# ------------------------------------------------------------------------------
# Test random effects for "class" by exactRLRT()
# ------------------------------------------------------------------------------


# We need to fit models without each of the random effect terms
jspr$craven <- jspr$raven - mean(jspr$raven)

mmodc <- lmer(math ~ craven * social + (1 | school : class), data = jspr)

mmods <- lmer(math ~ craven * social + (1 | school), data = jspr)




# ----------
# we can obtain p-value also by exactRLRT()
# m: fitted model under the alternative or, for testing in models with multiple variance components, the reduced model containing only the random effect to be tested.
# mA: the full model under the alternative for testing in models with multiple variance components
# m0: the model under the null for testing in models with multiple variance components

library(RLRsim)

exactRLRT(m = mmodc, mA = mmod_final, m0 = mmods)



# -->
# The evidence for a class effect is quite marginal.



# ------------------------------------------------------------------------------
# Test random effects for "school" by exactRLRT()
# ------------------------------------------------------------------------------

# In contrast, we can test for a school effect
exactRLRT(m = mmods, mA = mmod_final, m0 = mmodc)



# -->
# The school effect comes through strongly.
# It seems schools matter more than specific teachers.


