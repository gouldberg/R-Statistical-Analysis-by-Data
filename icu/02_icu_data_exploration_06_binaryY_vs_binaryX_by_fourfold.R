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
# Data Exploration:  fourfold
# binary Y vs. binary X
# ------------------------------------------------------------------------------

summary(ICU)



bin_var <- c("sex", "service", "cancer", "renal", "infect", "cpr", "previcu", "admit", "fracture", "po2", "ph", "pco", "bic", "white", "uncons")


graphics.off()

for(i in 1:length(bin_var)){
  eval(parse(text = paste0("tmp <- xtabs(~ died + ", bin_var[i], ", data = ICU)")))
  fourfold(tmp, fontsize = 16)
}


# -->
# The odds ratio over 1: renal / infect / cpr / admit / creatin / uncons
# The odds ratio under 1: service

