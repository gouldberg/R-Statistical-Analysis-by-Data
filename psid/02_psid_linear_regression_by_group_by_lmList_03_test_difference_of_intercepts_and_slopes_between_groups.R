setwd("//media//kswada//MyFiles//R//psid")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  psid
# ------------------------------------------------------------------------------

data("psid", package = "faraway")


str(psid)

dim(psid)


car::some(psid)



# ------------------------------------------------------------------------------
# Test the difference in income growth rates and incomes at the intercept (which is 1978) for men and women
# ------------------------------------------------------------------------------

t.test(slopes[psex == "M"], slopes[psex == "F"])


# -->
# We see that women have a significantly higher growth rate than men.



# ----------
t.test(intercepts[psex == "M"], intercepts[psex == "F"])



# -->
# We can see that men have significantly higher incomes.


