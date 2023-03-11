setwd("//media//kswada//MyFiles//R//pulp")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  pulp
# ------------------------------------------------------------------------------

data("pulp", package = "faraway")

str(pulp)

car::some(pulp)




# ------------------------------------------------------------------------------
# Null Hypothesis:  the variance between the operators is zero
# Parametric Bootstrap Approach
# ------------------------------------------------------------------------------

nullmod <- lm(bright ~ 1, data = pulp)


# The simulate function makes it simple to generate a sample from a model
( y <- simulate(nullmod) )


lrstat2 <- numeric(1000)

set.seed(123)

for(i in 1:1000){
  y <- unlist(simulate(nullmod))
  bnull <- lm(y ~ 1)
  balt <- lmer(y ~ 1 + (1|operator), pulp, REML = FALSE)
  lrstat2[i] <- as.numeric(2 * (logLik(balt) - logLik(bnull)))
}

lrstat2


hist(lrstat2)




# ----------
mean(lrstat2 < 0.00001)


# -->
# We see there is 70% chance that the likelihood for the null and alternatives are virtually identical giving an LRT statistic of practically zero.


# ----------
# Our estimated p-value is
mean(lrstat2 > 2.5684)


# standard error for this estimate
sqrt(mean(lrstat2 > 2.5684) * 0.981 / 1000)



# -->
# We can be fairly sure it is under 5%.

