setwd("//media//kswada//MyFiles//R//haireye_color")

packages <- c("dplyr", "datasets", "vcd", "vcdExtra", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  HairEyeColor
#  - frequencies of hair color, eye color, and sex from 592 students in a statistical course (Snee's 1974 sample)
# ------------------------------------------------------------------------------
data("HairEyeColor", package = "datasets")

dim(HairEyeColor)
str(HairEyeColor)

HairEyeColor


# collapse table to 2-way table (Hair(1) and Eye(2))
( haireye <- margin.table(HairEyeColor, 1:2) )



# ------------------------------------------------------------------------------
# 3-way and larger tables
# Generate models of 3 types independence
# ------------------------------------------------------------------------------
abbrev <- list(abbreviate = c(FALSE, FALSE, 1))

mod1 <- MASS::loglm(~ Hair + Eye + Sex, data = HEC)       # mutual
mod2 <- MASS::loglm(~ Hair * Sex + Eye * Sex, data = HEC) # conditional
mod3 <- MASS::loglm(~ Hair * Eye + Sex, data = HEC)       # joint

vcdExtra::LRstats(mod1, mod2, mod3)



# ------------------------------------------------------------------------------
# 3-way and larger tables
# Sequential plots and models
# ------------------------------------------------------------------------------
# Visual representation of the decomposition of the G^2 for mutual independence (total) as the sum of marginal and joint independence

# mosaic plot and G^2 statistic of mutual independence model
vcd::mosaic(HEC, expected = ~ Hair + Eye + Sex, legend = FALSE, labeling_args = abbrev, main = "Mutual")
mod1$lrt


# mosaic plot and G^2 statistic of marginal independence model
vcd::mosaic(~ Hair + Eye, data = HEC, shade = TRUE, legend = FALSE, main = "Marginal")
mod1$lrt - mod3$lrt


# mosaic plot and G^2 statistic of joint independence model
vcd::mosaic(HEC, expected = ~ Hair * Eye + Sex, legend = FALSE, labeling_args = abbrev, main = "Joint")
mod3$lrt



# ----------
# Example of generating the loglinear model formulae symbolically
for(nf in 2 : 5) print(vcdExtra::loglin2string(joint(nf, factors = LETTERS[1:5])))
for(nf in 2 : 5) print(vcdExtra::loglin2string(conditional(nf, factors = LETTERS[1:5]), sep = ""))
for(nf in 2 : 5) print(vcdExtra::loglin2formula(conditional(nf, factors = LETTERS[1:5])))


vcdExtra::loglin2formula(joint(3, table = HEC))
vcdExtra::loglin2string(joint(3, table = HEC))



# ----------
# Fit the collection of sequential models of a given type
HEC.mods <- seq_loglm(HEC, type = "joint")
HEC.mods
LRstats(HEC.mods)
