setwd("//media//kswada//MyFiles//R//haireye_color")

packages <- c("dplyr", "datasets", "vcd", "vcdExtra", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  HairEyeColor
# ------------------------------------------------------------------------------
data("HairEyeColor", package = "datasets")

data <- HairEyeColor

data


# collapse table to 2-way table (Hair(1) and Eye(2))
( haireye <- margin.table(data, 1:2) )



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
vcdExtra::loglin2formula(joint(3, table = HEC))

vcdExtra::loglin2string(joint(3, table = HEC))



# ----------
# Fit the collection of sequential models of a given type
HEC.mods <- seq_loglm(HEC, type = "joint")

HEC.mods

LRstats(HEC.mods)

