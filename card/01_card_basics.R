# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("//media//kswada//MyFiles//R//card")

packages <- c("dplyr", "Hmisc", "rms", "caret", "corrplot", "lattice", "ggplot2", "AER", "foreign", "car", "lmtest", "stargazer", "broom", "knitr", "POE5Rdata")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  card
# ------------------------------------------------------------------------------

card <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/card.dta")


str(card)


Hmisc::describe(card)

