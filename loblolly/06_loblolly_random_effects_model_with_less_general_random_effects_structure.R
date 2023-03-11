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



# ------------------------------------------------------------------------------
# model growth with random effects with corAR1, more order polynomials
# ------------------------------------------------------------------------------

lmc <- lmeControl(niterEM = 500, msMaxIter = 1000)



# More flexible model, so fourth and fifth order polynomials are also tried
m1 <- lme(height ~ age + I(age ^ 2) + I(age ^ 3) + I(age ^ 4), data = Loblolly, 
          random = list(Seed = ~age + I(age ^ 2) + I(age ^ 3)),
          correlation = corAR1(form = ~ age | Seed), control = lmc)



summary(m1)



# ----------
m2 <- lme(height ~ age + I(age ^ 2) + I(age ^ 3) + I(age ^ 4) + I(age ^ 5), data = Loblolly, 
          random = list(Seed = ~age + I(age ^ 2) + I(age ^ 3)),
          correlation = corAR1(form = ~ age | Seed), control = lmc)



summary(m2)



# ------------------------------------------------------------------------------
# Dropping corAR1
# ------------------------------------------------------------------------------

m3 <- lme(height ~ age + I(age ^ 2) + I(age ^ 3) + I(age ^ 4) + I(age ^ 5), data = Loblolly, 
          random = list(Seed = ~age + I(age ^ 2) + I(age ^ 3)), control = lmc)



summary(m3)



# ------------------------------------------------------------------------------
# Dropping random effect of the dependence of tree-specific growth on the cube of age
# ------------------------------------------------------------------------------

m4 <- lme(height ~ age + I(age ^ 2) + I(age ^ 3) + I(age ^ 4) + I(age ^ 5), data = Loblolly, 
          random = list(Seed = ~age + I(age ^ 2)),
          correlation = corAR1(form = ~ age | Seed), control = lmc)



summary(m4)




# ------------------------------------------------------------------------------
# Less general random effects structure:  b(j) ~ N(0, phi), phi is a diagonal matrix (with positive diagonal elements)
# ------------------------------------------------------------------------------

# pdDiag function indicates that the covariance matrix for the random effects at each level of Speed should have a (positive definite)
# diagonal structure

m5 <- lme(height ~ age + I(age ^ 2) + I(age ^ 3) + I(age ^ 4) + I(age ^ 5), data = Loblolly, 
          random = list(Seed = pdDiag(~age + I(age ^ 2) + I(age ^ 3))),
          correlation = corAR1(form = ~ age | Seed), control = lmc)



summary(m5)

