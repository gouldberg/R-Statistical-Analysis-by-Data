setwd("//media//kswada//MyFiles//R//wagepan")

packages <- c("dplyr", "Hmisc", "rms", "caret", "corrplot", "lattice", "ggplot2", "AER", "foreign", "car", "lmtest", "stargazer", "broom", "knitr", "POE5Rdata")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


library(plm)



# ------------------------------------------------------------------------------
# data:  wagepan
#   - balanced panel for n = 545 individuals over T = 8 years. It includes the index variables nr adn year for individuals and years, respectively.
#   - Since educ does not change over time, we cannot estimate its overall impact. However, we can interact it with time dummies to see how the impact changes over time.
# ------------------------------------------------------------------------------

wagepan <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/wagepan.dta")


str(wagepan)



# ----------
# Generate pdata.frame
wagepan.p <- pdata.frame(wagepan, index = c("nr", "year"))


pdim(wagepan.p)

glimpse(wagepan.p)


