setwd("//media//kswada//MyFiles//R//loblolloy")

packages <- c("dplyr", "lattice", "gamair")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# This tutorial is based on
# Chapter 2. GAMs in Practice: mgcv from "Generalized Additive Models An Introduction with R 2nd Edition" (by Simon N. Wood)


# ------------------------------------------------------------------------------
# data:  Loblloly
# ------------------------------------------------------------------------------

data(Loblolly, package = "gamair")


# Note that this is "nfnGroupedData" class
str(Loblolly)


head(Loblolly)



# ----------
# Seed was "ordered factor" --> changed to non-ordered factor
Loblolly$Seed <- as.factor(as.character(Loblolly$Seed))


# Without centering, polynomial terms can become highly correlated which can cause numerical difficulties
Loblolly$age <- Loblolly$age - mean(Loblolly$age)



# ----------
lmc <- lmeControl(niterEM = 500, msMaxIter = 1000)

m2 <- lme(height ~ age + I(age ^ 2) + I(age ^ 3) + I(age ^ 4) + I(age ^ 5), data = Loblolly, 
          random = list(Seed = ~age + I(age ^ 2) + I(age ^ 3)),
          correlation = corAR1(form = ~ age | Seed), control = lmc)


mod_obj <- m2



# ------------------------------------------------------------------------------
# Predicttion
# ------------------------------------------------------------------------------

# predicted coefficients
fixef(mod_obj)
ranef(mod_obj)



# ----------
# We specify the random effects part of the prediction as ~0 meaning that this term is not present.
predict(mod_obj, re.form = ~ 0)

predict(mod_obj)



# ----------
# Now we specify that the operator is 'a'
predict(mod_obj, newdata = data.frame(Seed = "301", age = 12))




# ------------------------------------------------------------------------------
# Plot predicted values
# ------------------------------------------------------------------------------

# augPred():  predicted values are obtained at the specified values of primary.
# If object has a grouping structure, predicted values are obtained for each group.

# This is model predictions from m2 at the individual tree level, overlaid on individual Loblolly pine growth data.
# The panel titles are the value of the Seed tree identifier.

augPred(mod_obj)

plot(augPred(mod_obj))





