setwd("//media//kswada//MyFiles//R//visual_acuity")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Visual Acuity
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



# ------------------------------------------------------------------------------
# Assess homogeneity of association (homofeneity of odds ratios)
# ------------------------------------------------------------------------------

woolf_test(dat)


# the analysis using woolf_test() is clearly non-significant, so we cannot reject homogeneity of association between male and female



