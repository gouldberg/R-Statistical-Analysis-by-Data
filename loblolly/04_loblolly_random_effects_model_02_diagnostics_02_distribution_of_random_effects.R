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

m0 <- lme(height ~ age + I(age ^ 2) + I(age ^ 3), data = Loblolly,
          random = list(Seed = ~age + I(age ^ 2) + I(age ^ 3)), control = lmc)


mod_obj <- m0



# ------------------------------------------------------------------------------
# Normal QQ plots for the predicted random effects
# ------------------------------------------------------------------------------


qqnorm(mod_obj, ~ ranef(.))



# -->
# The plots should look like correlated random scatters around straight lines,
# if the normality assumptions for the random effects are reasonable.

