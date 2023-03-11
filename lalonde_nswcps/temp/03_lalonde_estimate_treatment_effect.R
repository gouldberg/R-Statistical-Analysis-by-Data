setwd("//media//kswada//MyFiles//R//lalonde")

packages <- c("dplyr", "MatchIt", "Matching", "survey")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  lalonde
# ------------------------------------------------------------------------------

data("lalonde")

str(lalonde)



# ------------------------------------------------------------------------------
# Model based estimate
#   - standard parametric analysis and compute quantities of interest
# ------------------------------------------------------------------------------

# begin with nearest neighbour matching with a logistic regression-based propensity score, discarding with the hull.control option
m.out0 <- matchit(treat ~ age + educ + black + hispan + nodegree + married + re74 + re75, method = "nearest", discard = "hull.control", data = lalonde)

summary(m.out0)



# ----------
plot(m.out0)

plot(m.out0, type = "jitter")

plot(m.out0, type = "hist")



# ----------
# match.data() to create the matched data from the MatchIt output object (m.out)
# by excluding unmatched units from the original data, and including information produced by the particular matching procedure
# (i.e., primarily a newdata set, but also information that may result such as weights, subclasses, or the distance measure).

m.data0 <- match.data(m.out0)

head(m.data0)


# we run the parametric analysis (two variablesare dropped because they are exactly matched)
# fit a linear least squares model
library(Zelig)
z.out0 <- zelig(re78 ~ treat + age + educ + black + nodegree + re74 + re75, data = m.data0, model = "ls")

summary(z.out0)



# ----------
# the idea of Zelig is that statistical results are typically only intermediate quantities needed to compute your ultimate quantities of interest,
# which in the case of matching are usually causal inferences. 
# To get these causal quantities, we use Zelig’s other two commands.
# Thus, we can set the explanatory variables at their means (the default) and change the treatment variable from a 0 to a 1
x.out0 <- setx(z.out0, treat = 0)
x1.out0 <- setx(z.out0, treat = 1)


# finally compute the result and examine a summary
s.out0 <- sim(z.out0, x = x.out0, x1 = x1.out0)

summary(s.out0)



# ------------------------------------------------------------------------------
# Average treatment effect on the treated
# ------------------------------------------------------------------------------

m.out1 <- matchit(treat ~ age + educ + black + hispan + nodegree + married + re74 + re75, method = "nearest", data = lalonde)


plot(m.out1)

plot(m.out1, type = "jitter")

plot(m.out1, type = "hist")



# ----------
# fit a linear least squares model to the control group only.
z.out1 <- zelig(re78 ~ age + educ + black + hispan + nodegree + married + re74 + re75, data = match.data(m.out1, "control"),  model = "ls")



# ----------
# Next, we use the coefficients estimated in this way from the control group,
# and combine them with the values of the covariates set to the values of the treated units.
# We do this by choosing conditional prediction  (which means use the observed values) in setx().
# The sim() command does the imputation.

x.out1 <- setx(z.out1, data = match.data(m.out1, "treat"), cond = TRUE)

s.out1 <- sim(z.out1, x = x.out1)

summary(s.out1)                                                                                                                                                                                             



# ------------------------------------------------------------------------------
# Average treatment effect (overall)
# ------------------------------------------------------------------------------

z.out2 <- zelig(re78 ~ age + educ + black + hispan + nodegree + married + re74 + re75, data = match.data(m.out1, "treat"), model = "ls")


# ----------
# We then conduct the same simulation procedure in order to impute the counterfactual outcome for the control group
x.out2 <- setx(z.out2, data = match.data(m.out1, "control"), cond = TRUE)

s.out2 <- sim(z.out2, x = x.out2)



# ----------
# In this calculation, Zelig is computing the difference between observed and the expected values.
# This means that the treatment effect for the control units is the effect of control (observed control outcome minus the imputed outcome under treatment from the model).
# Hence, to combine treatment effects just reverse the signs of the estimated treatment effect of controls.

ate.all <- c(s.out1$qi$ATT.ev, - s.out2$qi$att.ev)

mean(ate.all)

sd(ate.all)

quantile(ate.all, c(0.025, 0.975))



# ------------------------------------------------------------------------------
# Subclassification
#   - In subclassification, the average treatment effect estimates are obtained separately for each subclass, 
#     and then aggregated for an overall estimate.
#   - Estimating the treatment effects separately for each subclass, and then aggregating across sub-classes, 
#     can increase the robustness of the ultimate results since the parametric analysis within each subclass requires only local rather than global assumptions.
#     However, fewer observations are obviously available within each subclass, and so this option is normally chosen for larger data sets.
# ------------------------------------------------------------------------------

m.out2 <- matchit(treat ~ age + educ + black + hispan + nodegree + married + re74 + re75, data = lalonde, method = "subclass", subclass = 4)



# fit a linear regression within each subclass by controlling for the estimated propensity score (called distance) and other covariates. 
z.out3 <- zelig(re78 ~ re74 + re75 + distance, data = match.data(m.out2, "control"), model = "ls", by = "subclass")



# ----------
x.out3 <- setx(z.out3, data = match.data(m.out2, "treat"), fn = NULL, cond = TRUE)

s.out3 <- sim(z.out3, x = x.out3)

summary(s.out3)


# get summary result for each subclass specifying the subclass
summary(s.out3, subset = 2)




