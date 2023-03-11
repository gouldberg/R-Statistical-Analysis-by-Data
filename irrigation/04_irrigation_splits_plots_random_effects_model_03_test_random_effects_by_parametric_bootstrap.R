setwd("//media//kswada//MyFiles//R//irrigation")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  irrigation
# ------------------------------------------------------------------------------

data("irrigation", package = "faraway")

str(irrigation)

car::some(irrigation)




# ------------------------------------------------------------------------------
# Test random effects (test the significance of the field) by parametric bootstrap
# ------------------------------------------------------------------------------

rmod <- lmer(yield ~ irrigation * variety + (1 | field), data = irrigation)


nlmod <- lm(yield ~ irrigation * variety, data = irrigation)



# ----------
# We need to specify the nondefault REML option for null model to ensure that the LRT is computed correctly.
( lrstatf_ref <- as.numeric(2 * (logLik(rmod) - logLik(nlmod, REML = TRUE))) )


lrstatf <- numeric(1000)

for(i in 1:1000){
  ryield <- unlist(simulate(nlmod))
  nlmodr <- lm(ryield ~ irrigation * variety, data = irrigation)
  rmodr <- refit(rmod, ryield)
  lrstatf[i] <- as.numeric(2 * (logLik(rmodr) - logLik(nlmodr, REML = TRUE)))
}



# ----------
mean(lrstatf < 0.00001)


# -->
# We can see from this that the LRT is clearly not X^2(1) distributed. Even the nonzero values seem to have some other distribution.



mean(lrstatf > lrstatf_ref)


# -->
# We find significant field effect



# ------------------------------------------------------------------------------
# Test random effects (test the significance of the blends) by exactRLRT()
# ------------------------------------------------------------------------------

# we can obtain p-value also by exactRLRT()

library(RLRsim)

exactRLRT(rmod)



# -->
# We see that the fields do seem to vary as the result is clearly significant.

