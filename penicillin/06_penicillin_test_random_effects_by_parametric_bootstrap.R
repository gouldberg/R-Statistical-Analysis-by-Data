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
# Test random effects (test the significance of the blends) by parametric bootstrap
# ------------------------------------------------------------------------------

# We may wish to examine the blocking effects for information useful for the design of future experiments.
# We can fit the model with and without random effects and compute the LRT.

rmod <- lmer(yield ~ treat + (1 | blend), data = penicillin)

nlmod <- lm(yield ~ treat, data = penicillin)


# We need to specify the nondefault REML option for null model to ensure that the LRT is computed correctly.
( lrstatf_ref <- as.numeric(2 * (logLik(rmod) - logLik(nlmod, REML = TRUE))) )


lrstatf <- numeric(1000)

for(i in 1:1000){
  ryield <- unlist(simulate(nlmod))
  nlmodr <- lm(ryield ~ treat, data = penicillin)
  rmodr <- refit(rmod, ryield)
  lrstatf[i] <- as.numeric(2 * (logLik(rmodr) - logLik(nlmodr, REML = TRUE)))
}



# ----------
mean(lrstatf < 0.00001)


# -->
# We can see from this that the LRT is clearly not X^2(1) distributed. Even the nonzero values seem to have some other distribution.


mean(lrstatf > lrstatf_ref)


# -->
# We find a significant blend effect ..?



# ------------------------------------------------------------------------------
# Test random effects (test the significance of the blends) by exactRLRT()
# ------------------------------------------------------------------------------

# we can obtain p-value also by exactRLRT()

library(RLRsim)

exactRLRT(rmod)



# -->
# In this example, we saw no major advantage in modeling the blocks as random effects, so we might prefer to use the fixed effects analysis.


