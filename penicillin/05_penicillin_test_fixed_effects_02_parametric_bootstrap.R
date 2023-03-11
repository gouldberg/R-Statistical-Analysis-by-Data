setwd("//media//kswada//MyFiles//R//penicillin")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  penicillin
# ------------------------------------------------------------------------------
data("penicillin", package = "faraway")

str(penicillin)

car::some(penicillin)



# ------------------------------------------------------------------------------
# Test fixed effects by parametric bootstrap
# ------------------------------------------------------------------------------

lrstat <- numeric(1000)


for(i in 1:1000){
  ryield <- unlist(simulate(nmod))
  # We use refit to speed up the computation
  nmodr <- refit(nmod, ryield)
  amodr <- refit(amod, ryield)
  lrstat[i] <- as.numeric(2 * (logLik(amodr) - logLik(nmodr)))
}


qqplot(qchisq(lrstat, df = 3), y,
       main = expression("Q-Q plot for" ~~ {chi^2}[nu == 3]))

# -->
# QQ plot of these simuated LRT values indicates that this is a poor approximation


# ----------
( lrstat_ref <- as.numeric(2 * (logLik(amod) - logLik(nmod))) )

mean(lrstat > lrstat_ref)


# -->
# much closer to the F-test result than X^2(3) approximation.



# ------------------------------------------------------------------------------
# Test fixed effects by parametric bootstrap by pbkrtest()
#   - The pbkrtest package offers a convenient way to perform the parametric bootstraop for fixed effect terms
# ------------------------------------------------------------------------------
library(pbkrtest)

amod <- lmer(yield ~ treat + (1 | blend), data = penicillin, REML = FALSE)
nmod <- lmer(yield ~ 1 + (1 | blend), data = penicillin, REML = FALSE)


pmod <- PBmodcomp(amod, nmod)

summary(pmod)


# -->
# The parametric bootstrap p-value is 0.3367, which is similar to our previous results.
