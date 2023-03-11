setwd("//media//kswada//MyFiles//R//arthritis")

packages <- c("dplyr", "vcd", "MASS", "datasets")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Arthritis
# ------------------------------------------------------------------------------
data("Arthritis", package = "vcd")

data <- Arthritis

data


tab2 <- xtabs(~ Treatment + Improved + Sex, data = data)



# ------------------------------------------------------------------------------
# Assess homogeneity of association (homofeneity of odds ratios)
# ------------------------------------------------------------------------------

woolf_test(tab2)


# --> Even though we found in the CMH analysis that the association between Treatment and Improved was stronger for females than males,
# the analysis using woolf_test() is clearly non-significant, so we cannot reject homogeneity of association



# ------------------------------------------------------------------------------
# 3-way table, the hypothesis of homogeneity of association among three variables A, B, and C can be stated as the loglinear model
# of no three-way association, [AB][AC][BC].
# ------------------------------------------------------------------------------

loglm(~ (Treatment + Improved + Sex)^2, data = tab2)


# --> Consistent with the Woolf test, the interaction terms are not significant.
# --> We cannot reject homofeneity of association.



