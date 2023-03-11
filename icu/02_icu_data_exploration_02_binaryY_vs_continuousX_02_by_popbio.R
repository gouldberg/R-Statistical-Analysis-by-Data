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
# Data Exploration:  Marginal plots by popbio
# binary Y (ratio) vs. continuous X
# ------------------------------------------------------------------------------

library(popbio)


par(mfrow=c(1,1))


# default:  log.mod = 1  (logistic regression)
with(ICU, logi.hist.plot(age, died == "Yes", type = "hist", rug = TRUE,
                          counts = TRUE, ylabel = "Probability", xlab = "Age", col.hist = "lightblue"))


par(mfrow=c(1,1))
with(ICU, logi.hist.plot(systolic, died == "Yes", type = "hist", rug = TRUE,
                          counts = TRUE, ylabel = "Probability", xlab = "Systolic", col.hist = "lightblue"))


par(mfrow=c(1,1))
with(ICU, logi.hist.plot(hrtrate, died == "Yes", type = "hist", rug = TRUE,
                          counts = TRUE, ylabel = "Probability", xlab = "Hrtrate", col.hist = "lightblue"))


# -->
# age and systolic seems to have some relationship with response (died)

