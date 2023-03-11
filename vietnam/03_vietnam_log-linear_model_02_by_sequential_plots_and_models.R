setwd("//media//kswada//MyFiles//R//vietnam")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS", "gpairs")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Vietnam
# ------------------------------------------------------------------------------

data("Vietnam", package = "vcdExtra")

dim(Vietnam)
str(Vietnam)


car::some(Vietnam)


# frequency form in dataframe to table
( vietnam.tab <- xtabs(Freq ~ sex + year + response, data = Vietnam) )

str(vietnam.tab)



# ------------------------------------------------------------------------------
# Sequential plots and models
# Example of generating the loglinear model character strings or formulae symbolically
#  - mutual
#  - joint
#  - conditional
#  - markov
#  - saturated
# ------------------------------------------------------------------------------
# mututal independence
for(nf in 2 : 5) print(vcdExtra::loglin2string(mutual(nf, factors = LETTERS[1:5])))
for(nf in 2 : 5) print(vcdExtra::loglin2formula(mutual(nf, factors = LETTERS[1:5])))



# joint independence
for(nf in 2 : 5) print(vcdExtra::loglin2string(joint(nf, factors = LETTERS[1:5])))
for(nf in 2 : 5) print(vcdExtra::loglin2formula(joint(nf, factors = LETTERS[1:5])))



# conditional independence
for(nf in 2 : 5) print(vcdExtra::loglin2string(conditional(nf, factors = LETTERS[1:5]), sep = ""))
for(nf in 2 : 5) print(vcdExtra::loglin2formula(conditional(nf, factors = LETTERS[1:5])))



# markov
for(nf in 2 : 5) print(vcdExtra::loglin2string(markov(nf, factors = LETTERS[1:5]), sep = ""))
for(nf in 2 : 5) print(vcdExtra::loglin2formula(markov(nf, factors = LETTERS[1:5])))



# saturated
for(nf in 2 : 5) print(vcdExtra::loglin2string(saturated(nf, factors = LETTERS[1:5]), sep = ""))
for(nf in 2 : 5) print(vcdExtra::loglin2formula(saturated(nf, factors = LETTERS[1:5])))



# ------------------------------------------------------------------------------
# Sequential plots and models
# Applied to our hypothesis
# ------------------------------------------------------------------------------
vcdExtra::loglin2formula(joint(3, table = TV3))

vcdExtra::loglin2string(joint(3, table = TV3))



# ----------
# Fit the collection of sequential models of a given type
vie.mods <- seq_loglm(vietnam.tab, type = "joint")

vie.mods

LRstats(vie.mods)



# ----------
# Fit the collection of sequential models of a given type
vie.mods2 <- seq_loglm(aperm(vietnam.tab, c(3,2,1)), type = "conditional")

vie.mods2

LRstats(vie.mods2)

