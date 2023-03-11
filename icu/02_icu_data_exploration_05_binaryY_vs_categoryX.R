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



# ------------------------------------------------------------------------------
# Data Exploration: odds ratio
# binary Y  vs.  categorical X
# ------------------------------------------------------------------------------

fac_var <- c("sex", "service", "cancer", "renal", "infect", "cpr", "previcu", "admit", "fracture", "po2", "ph", "pco", "bic", "creatin", "white", "uncons")


# confidence interval of odds ratio by each binary factor variable 
for(i in 1:length(fac_var)){
  eval(parse(text = paste0("tmp <- xtabs(~ died + ", fac_var[i], ", data = ICU)")))
  #  fourfold(tmp, fontsize = 16)
  print(paste0("var: ", fac_var[i]))
  print(round(confint(loddsratio(tmp, log = FALSE)), digits = 3))
}


# -->
# The odds ratio over 1: renal / infect / cpr / admit / creatin / uncons
# The odds ratio under 1: service



# ------------------------------------------------------------------------------
# The relationship of categorical variables combination X against binaryY
# ------------------------------------------------------------------------------

vcd::doubledecker(died ~ cancer + admit, data = ICU2)

vcd::doubledecker(died ~ admit + uncons, data = ICU2)




