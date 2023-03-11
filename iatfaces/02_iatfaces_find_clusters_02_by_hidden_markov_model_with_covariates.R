setwd("//media//kswada//MyFiles//R//iatfaces")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  iatfaces
# ------------------------------------------------------------------------------

data("iatfaces", package = "MPsychoR")


str(iatfaces)


p1dat <- subset(iatfaces, id == 1)
p2dat <- subset(iatfaces, id == 2)
p3dat <- subset(iatfaces, id == 3)
p4dat <- subset(iatfaces, id == 4)



# ------------------------------------------------------------------------------
# Fit HMM with covariate block, 1-state model
# ------------------------------------------------------------------------------

library(depmixS4)


# Specifying a one-state HMM with block z(t) as effect on log-latency implies fitting a simple regression of the form
# Y(t) = mu + beta1 * z(t) + error(t)
p1obj2 <- depmix(log(latency) ~ block, data = p1dat, ns = 1)


p1fit2a <- fit(p1obj2, verbose = TRUE)

summary(p1fit2a, which = "response")

summary(lm(log(latency) ~ block, data = p1dat))



# ------------------------------------------------------------------------------
# Fit HMM with covariate block, 2-state model
#  - Clustering and the regression fit happen simultaneously, thus, we cannot achieve identical results with simple lm calls anymore.
# ------------------------------------------------------------------------------

# Y(t) = mu(i) + beta1(i) * z(t) + error(t)  (i = 1, 2)
p1obj3 <- depmix(log(latency) ~ block, data = p1dat, ns = 2)


p1fit2b <- fit(p1obj3, verbose = TRUE)

summary(p1fit2b)
summary(p1fit2b, which = "response")



# -->
# The trainsition probabilities chance drastically compared to the 2-state model with no covariate.
# This is due to the fact that the states are fully determined by the condition switch.

# The transition probabilities are close to a coin toss, since we account for the block effect in the regression specification.



# ----------
# Of course, the states have to be interpreted differently compared to the HMM with no covariates.
# The BIC suggests that we should go with the on-state mode:
c(BIC(p1fit2a), BIC(p1fit2b))




# ------------------------------------------------------------------------------
# Fit HMM with covariate block, 2-state model for remaining person
# ------------------------------------------------------------------------------

p2obj3 <- depmix(log(latency) ~ block, data = p2dat, ns = 2)
p3obj3 <- depmix(log(latency) ~ block, data = p3dat, ns = 2)
p4obj3 <- depmix(log(latency) ~ block, data = p4dat, ns = 2)

p2fit2b <- fit(p2obj3, verbose = TRUE)
p3fit2b <- fit(p3obj3, verbose = TRUE)
p4fit2b <- fit(p4obj3, verbose = TRUE)


summary(p2fit2b)
summary(p3fit2b)
summary(p4fit2b)


