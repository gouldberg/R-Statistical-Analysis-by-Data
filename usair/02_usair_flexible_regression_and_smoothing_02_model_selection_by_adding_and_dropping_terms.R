setwd("//media//kswada//MyFiles//R//usair")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  usair
# ------------------------------------------------------------------------------
data("usair", package = "gamlss.data")


str(usair)

car::some(usair)



# ------------------------------------------------------------------------------
# Single term deletion by drop1()
#   - By default add1() and drop1() report both GAIC and the likelihood ratio test with its X^2 values,
#     while addterm() and dropterm() report only the GAIC
# ------------------------------------------------------------------------------

# Fit gamma distribution with all variables
mod1 <- gamlss(y ~ ., data = usair, family = GA)


summary(mod1)



# ----------
dd <- drop1(mod1)

dd



# ----------
# Note that the same output is obtained from
dropterm(mod1, test = "Chisq")


# -->
# This is the generalized likelihood ratio test statistic (LRT) and its Chi-squared p-values (Pr(Chi))
# for removing each of the six variables from the full model.
# Given all other linear terms in the model, the variable x6 is the first to be dropped since it has the highest p-values



# ------------------------------------------------------------------------------
# add 2-way interaction term by add1()
# ------------------------------------------------------------------------------

# scope is a formula specifying a maximal model which should include the current one.
add1(mod1, scope = ~(x1 + x2 + x3 + x4 + x5 + x6)^2)



# ------------------------------------------------------------------------------
# Simgle term addition by addterm():  add each smoother one at a time
# ------------------------------------------------------------------------------

# Create null model containing only the intercept
mod0 <- gamlss(y ~ 1, data = usair, family = GA)


# each smoother is added one at a time
addterm(mod0, scope = ~pb(x1) + pb(x2) + pb(x3) + pb(x4) + pb(x5) + pb(x6), test = "Chisq")


