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

m0_2 <- lme(height ~ age + I(age ^ 2) + I(age ^ 3), data = Loblolly, 
          random = list(Seed = ~age + I(age ^ 2) + I(age ^ 3)),
          correlation = corAR1(form = ~ age | Seed), control = lmc)

mod_obj <- m0_2




# ------------------------------------------------------------------------------
# Standardized residuals against fitted value and age, with by group (Seed)
# ------------------------------------------------------------------------------

plot(mod_obj)


plot(mod_obj, resid(., type = "p") ~ fitted(.) | Seed, abline = 0, lty = 2, col = gray(0.4), cex = 0.8, pch = 20)

plot(mod_obj, resid(., type = "p") ~ age | Seed, abline = 0, lty = 2, col = gray(0.4), cex = 0.8, pch = 20)



# ------------------------------------------------------------------------------
# Distribution of Standardized residuals by group (Seed)
# ------------------------------------------------------------------------------

plot(mod_obj, Seed ~ resid(., type = "p"))


