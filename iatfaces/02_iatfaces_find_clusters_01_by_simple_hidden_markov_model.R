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
# Fit the first participant an HMM using the depmixS4 to find latency clusters
# ------------------------------------------------------------------------------

library(depmixS4)


set.seed(123)

p1obj1 <- depmix(log(latency) ~ 1, data = p1dat, nstates = 1)
p1obj2 <- depmix(log(latency) ~ 1, data = p1dat, nstates = 2)
p1obj3 <- depmix(log(latency) ~ 1, data = p1dat, nstates = 3)
p1obj4 <- depmix(log(latency) ~ 1, data = p1dat, nstates = 4)


p1fit1 <- fit(p1obj1)
p1fit2 <- fit(p1obj2)
p1fit3 <- fit(p1obj3)
p1fit4 <- fit(p1obj4)



# ------------------------------------------------------------------------------
# Goodness-of-fit assessment
# ------------------------------------------------------------------------------

c(BIC(p1fit1), BIC(p1fit2), BIC(p1fit3), BIC(p1fit4))


# -->
# two-state solution provides clearly the best fit




# ------------------------------------------------------------------------------
# Model
# ------------------------------------------------------------------------------

summary(p1fit2)


# -->
# the transition matrix reflects the dynamics between the 2 states.
# If the participant is in state 1, he/she tends to stay there (= 0.975).
# There is a low probability for switching to state 2 (= 0.025).
# Once switched to state 2, he/she remains there (= 1.0), and there is virtually no chance to go back to state 1 (= 0.0)


# Since we fixed the number of latent states to K = 2, the parameters result from the simple regression equation: Y(t) = mu(i) + error(t) (i = 1, 2)
# We get two means on the log-latency scale (intercept) and two standard deviations.
# In the first state the average latency is much lower (fast-responding state) than in the second state (slow-responding state)


# ----------
round(posterior(p1fit2)[35:45, ], 3)


# -->
# The HMM fits a state posterior probability for each measurement point.
# For each of the 80 measurements, we get a probability to be in state 1 and state 2, respectively.
# The observations around the condition switch at time point 41. 



# ----------
# Evaluatge the hard state assignment by cross-classification with the experimental condition variable.
table(state = p1fit2@posterior$state, block = p1dat$block)




# ------------------------------------------------------------------------------
# Fit corresponding two-state models on the remaining data
# ------------------------------------------------------------------------------

set.seed(123)

p2obj2 <- depmix(log(latency) ~ 1, data = p2dat, nstates = 2)
p3obj2 <- depmix(log(latency) ~ 1, data = p3dat, nstates = 2)
p4obj2 <- depmix(log(latency) ~ 1, data = p4dat, nstates = 2)


p2fit <- fit(p2obj2)
p3fit <- fit(p3obj2)
p4fit <- fit(p4obj2)



# ----------
# Evaluatge the hard state assignment by cross-classification with the experimental condition variable.

table(state = p2fit@posterior$state, block = p1dat$block)
table(state = p3fit@posterior$state, block = p2dat$block)
table(state = p4fit@posterior$state, block = p3dat$block)



# -->
# Person 3 behaves very similar to person 1.
# The other two individuals show a somewhat different state pattern, not in line with the experimental condition.



# ----------
summary(p3fit)

summary(p2fit)
summary(p4fit)


