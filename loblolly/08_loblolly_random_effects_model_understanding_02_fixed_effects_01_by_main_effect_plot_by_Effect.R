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
# Model understanding:  Fixed Effects
# ------------------------------------------------------------------------------

library(effects)

plot(predictorEffects(mod_obj))


# ----------
# plot main effects with partial residuals for numeric variables with smoothed loess curve
plot(predictorEffects(mod_obj, residuals = TRUE), partial.residuals = list(cex = 0.35, col = gray(0.5), lty = 2))

# eff2 <- effects::allEffects(mod_obj, partial.residuals = TRUE)


