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
# Standardized residuals against fitted value and age, with by group (Seed)
# ------------------------------------------------------------------------------

plot(m2)

plot(m5)


# -->
# residuals for m2 is better


plot(m2, resid(., type = "p") ~ fitted(.) | Seed, abline = 0, lty = 2, col = gray(0.4), cex = 0.8, pch = 20)

plot(m5, resid(., type = "p") ~ fitted(.) | Seed, abline = 0, lty = 2, col = gray(0.4), cex = 0.8, pch = 20)



plot(m2, resid(., type = "p") ~ age | Seed, abline = 0, lty = 2, col = gray(0.4), cex = 0.8, pch = 20)

plot(m5, resid(., type = "p") ~ age | Seed, abline = 0, lty = 2, col = gray(0.4), cex = 0.8, pch = 20)



# ------------------------------------------------------------------------------
# Distribution of Standardized residuals by group (Seed)
# ------------------------------------------------------------------------------

plot(m2, Seed ~ resid(., type = "p"))

plot(m5, Seed ~ resid(., type = "p"))


# -->
# here residuals distribution for m5 is better

