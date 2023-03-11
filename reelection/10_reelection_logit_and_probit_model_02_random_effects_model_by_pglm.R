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
# fit logit and probit models by pglm
# ------------------------------------------------------------------------------

library(pglm)


elect.pl <- pglm(reelect ~ ddefterm + ddefey + gdppc + dev + nd + maj,
               data = Reelection,
               family = binomial(link = "logit"), subset = narrow)



elect.pp <- pglm(reelect ~ ddefterm + ddefey + gdppc + dev + nd + maj,
                 data = Reelection,
                 family = binomial(link = "probit"), subset = narrow)




# ----------
library(texreg)
# screenreg(list(plogit = elect.pl, pprobit = elect.pp), digits = 3)

car::compareCoefs(elect.l, elect.p, elect.pl, elect.pp)

screenreg(list(logit = elect.l, probit = elect.p), digits = 3)



# -->
# The probability of being reelected is larger in developing and newly democratic countries and for majoritarian electral systems
# The growth rate of GDP also has the predicted positive effect on the probability of being reelected.
# The coefficients of the two fiscal policy covariates are positive, which means that expansionary fiscal policies before elections
# do not have a systematic positive effect on the probablity of the incumbent being reelected.
# On the contrary, the results indicate that voters tend to sanciton such policies.




