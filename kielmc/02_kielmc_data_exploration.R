setwd("//media//kswada//MyFiles//R//kielmc")

packages <- c("dplyr", "foreign")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  kielmc
#   - Wooldridge (2016, Section 13.2) example:  effect of a garbage incinerator's location or housing prices
# ------------------------------------------------------------------------------

# library(foreign)
# kielmc <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/kielmc.dta")
# readr::write_tsv(kielmc, path = "kielmc.txt")

kielmc <- readr::read_tsv("kielmc.txt")


dim(kielmc)

str(kielmc)


car::some(kielmc)



# ------------------------------------------------------------------------------
# data exploration:  price distribution by nearinc + year
# ------------------------------------------------------------------------------

psych::describe(kielmc)

Hmisc::describe(kielmc)



# ----------
# distribution of the prices
lattice::bwplot(rprice ~ as.factor(year) | as.factor(nearinc), data = kielmc)

lattice::bwplot(lrprice ~ as.factor(year) | as.factor(nearinc), data = kielmc)



# ----------
with(kielmc, by(rprice, nearinc, summary))

with(kielmc, by(rprice, nearinc + year, summary))




