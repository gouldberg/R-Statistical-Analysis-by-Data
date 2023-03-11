setwd("//media//kswada//MyFiles//R//icu")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Death in the ICU
# ------------------------------------------------------------------------------

data("ICU", package = "vcdExtra")

str(ICU)

dim(ICU)


car::some(ICU)



# ----------
# Here we do not apply "rage" and "coma"
# race, coma is represented initially as 3-level factors, but the recoded to binary variables (white, uncons)
# we apply binary variables

ICU2 <- ICU[, -c(4, 20)]

str(ICU2)



# ----------
levels(ICU2$cancer) <- c("-", "Cancer")
levels(ICU2$admit) <- c("-", "Emerg")
levels(ICU2$uncons) <- c("-", "Uncons")

icu.glm2 <- glm(died ~ age + cancer + admit + uncons, data = ICU2, family = binomial)



# ------------------------------------------------------------------------------
# Conditional effect plot
# ------------------------------------------------------------------------------

library(effects)



# effect plots for several predictors jointly or full-model plots

plot(Effect(c("admit", "cancer"), icu.glm2), 
     confint = list(style = "bars"),
     lines = list(multiline = TRUE, lty = c(2,1), col = c(gray(0.7), "black")))


plot(Effect(c("admit", "age"), icu.glm2), 
     confint = list(style = "bands"), 
     lines = list(multiline = TRUE, lty = c(2,1), col = c(gray(0.7), "black")))


plot(Effect(c("admit", "cancer", "uncons"), icu.glm2), 
     confint = list(style = "bars"), 
     lines = list(multiline = TRUE, lty = c(2,1), col = c(gray(0.7), "black")))

