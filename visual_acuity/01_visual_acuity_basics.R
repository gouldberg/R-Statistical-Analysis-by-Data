setwd("//media//kswada//MyFiles//R//visual_acuity")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Visual Acuity
#  - In World War II, all workers in the UK Royal Ordnance factories were given test of visual acuity (unaided distance vision) of
#    their left and right eyes on a 1 (high) to 4 (low) scale.
#    The dataset VisualAcuity in vcd gives the results for 10,719 workers (3,242 men, 7,477 women) aged 30-39
# ------------------------------------------------------------------------------
data("VisualAcuity", package = "vcd")

data <- VisualAcuity

car::some(data)


# Convert frequency data frame to table form
dat <- xtabs(Freq ~ right + left + gender, data = VisualAcuity)

dimnames(dat)[1:2] <- list(c("high", 2, 3, "low"))

names(dimnames(dat))[1:2] <- paste(c("Right", "Left"), "eye grade")

dat


structable(aperm(dat))


