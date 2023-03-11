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
# model growth with random effects with corAR1
# ------------------------------------------------------------------------------

lmc <- lmeControl(niterEM = 500, msMaxIter = 1000)


# Some degree of autocorrelation in the random component of height
# corAR1:  autoregressive process of order 1
m0_2 <- lme(height ~ age + I(age ^ 2) + I(age ^ 3), data = Loblolly, 
          random = list(Seed = ~age + I(age ^ 2) + I(age ^ 3)),
          correlation = corAR1(form = ~ age | Seed), control = lmc)



# -----------
summary(m0_2)


