setwd("//media//kswada//MyFiles//R//tv")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  TV viewing data
# ------------------------------------------------------------------------------
data("TV", package = "vcdExtra")


TV

dim(TV)

str(TV)



# ----------
TV2 <- margin.table(TV, c(1,3))

TV2



# ----------
TV.df <- as.data.frame.table(TV)

levels(TV.df$Time) <- rep(c("8", "9", "10"), c(4, 4, 3))

TV3 <- xtabs(Freq ~ Day + Time + Network, TV.df)

TV3




# ------------------------------------------------------------------------------
# Sequential plots and models
# Example of generating the loglinear model character strings or formulae symbolically
#  - mutual
#  - joint
#  - conditional
#  - markov
#  - saturated
# ------------------------------------------------------------------------------

library(vcdExtra)


# mututal independence
for(nf in 2 : 5) print(loglin2string(mutual(nf, factors = LETTERS[1:5])))

for(nf in 2 : 5) print(loglin2formula(mutual(nf, factors = LETTERS[1:5])))



# joint independence
for(nf in 2 : 5) print(loglin2string(joint(nf, factors = LETTERS[1:5])))

for(nf in 2 : 5) print(loglin2formula(joint(nf, factors = LETTERS[1:5])))



# conditional independence
for(nf in 2 : 5) print(loglin2string(conditional(nf, factors = LETTERS[1:5]), sep = ""))

for(nf in 2 : 5) print(loglin2formula(conditional(nf, factors = LETTERS[1:5])))



# markov
for(nf in 2 : 5) print(loglin2string(markov(nf, factors = LETTERS[1:5]), sep = ""))

for(nf in 2 : 5) print(loglin2formula(markov(nf, factors = LETTERS[1:5])))



# saturated
for(nf in 2 : 5) print(loglin2string(saturated(nf, factors = LETTERS[1:5]), sep = ""))

for(nf in 2 : 5) print(loglin2formula(saturated(nf, factors = LETTERS[1:5])))



# ------------------------------------------------------------------------------
# Sequential plots and models
# Applied to our hypothesis
# ------------------------------------------------------------------------------

loglin2formula(joint(3, table = TV3))


loglin2string(joint(3, table = TV3))




# ----------
# Fit the collection of sequential models of a given type

tv.mods <- seq_loglm(TV3, type = "joint")


tv.mods


LRstats(tv.mods)



# ----------
# Fit the collection of sequential models of a given type

tv.mods2 <- seq_loglm(TV3, type = "conditional")


tv.mods2


LRstats(tv.mods2)

