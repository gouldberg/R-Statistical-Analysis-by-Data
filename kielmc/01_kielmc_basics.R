setwd("//media//kswada//MyFiles//R//kielmc")

packages <- c("dplyr", "foreign")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  kielmc
#   - Wooldridge (2016, Section 13.2) example:  effect of a garbage incinerator's location or housing prices
#   - 1978:  before there were any rumors about the new incinerator
#     1981:  when the construction of garbage incinerator began
# ------------------------------------------------------------------------------

# library(foreign)
# kielmc <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/kielmc.dta")
# readr::write_tsv(kielmc, path = "kielmc.txt")

kielmc <- readr::read_tsv("kielmc.txt")


dim(kielmc)

str(kielmc)


car::some(kielmc)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
