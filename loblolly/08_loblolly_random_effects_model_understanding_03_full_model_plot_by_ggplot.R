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
# Full-model plots by ggplots
# ------------------------------------------------------------------------------

lob.fitp <- cbind(Loblolly, height_pred = predict(mod_obj, type = "response"))

# lob.fitp <- fortify(mod_obj)

head(lob.fitp)




library(ggplot2)


graphics.off()

gg <- ggplot(lob.fitp, aes(x = age, y = height_pred)) + theme_bw() + geom_point(colour = "black", size = 1.5)

gg + facet_wrap(~ Seed) + stat_smooth()
