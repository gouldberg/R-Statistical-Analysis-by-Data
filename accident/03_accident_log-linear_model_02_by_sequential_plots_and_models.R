setwd("//media//kswada//MyFiles//R//accident")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS", "gpairs")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Accident
# ------------------------------------------------------------------------------

data("Accident", package = "vcdExtra")

dim(Accident)

str(Accident)


car::some(Accident)


# frequency form in dataframe to table
( acci.tab <- xtabs(Freq ~ age + result + mode + gender, data = Accident) )



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
vcdExtra::loglin2formula(joint(4, table = acci.tab, with = 2))
vcdExtra::loglin2string(joint(4, table = acci.tab, with = 2))

vcdExtra::loglin2formula(conditional(4, table = acci.tab, with = c(1,4)))
vcdExtra::loglin2string(conditional(4, table = acci.tab, with = c(1,4)))



# ----------
# Fit the collection of sequential models of a given type
acci.mods <- seq_loglm(aperm(acci.tab, c(2,1,3,4)), type = "joint")

acci.mods

LRstats(acci.mods)



# ----------
# Fit the collection of sequential models of a given type
acci.mods2 <- seq_loglm(aperm(acci.tab, c(2,1,3,4)), type = "conditional")

acci.mods2

LRstats(acci.mods2)

