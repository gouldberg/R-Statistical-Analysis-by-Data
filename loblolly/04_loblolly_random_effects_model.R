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
# model growth with ranbdom effects of Seed
# ------------------------------------------------------------------------------

library(nlme)

# This produces errors.
# m0 <- lme(height ~ age + I(age ^ 2) + I(age ^ 3) + (1|Seed), data = Loblolly)


# ----------
# default setting method fails without some adjustment
# lme fits start by using the EM algorithm to get reasonble close to the optimal parameter estimates, and then switch to Newton's method,
# which converges more quickly.
# Note that niterEM should rarely be incread from its default 0 when calling gamm (for future reference)

lmc <- lmeControl(niterEM = 500, msMaxIter = 1000)


m0 <- lme(height ~ age + I(age ^ 2) + I(age ^ 3), data = Loblolly,
          random = list(Seed = ~age + I(age ^ 2) + I(age ^ 3)), control = lmc)


summary(m0)


