setwd("//media//kswada//MyFiles//R//vietnam")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS", "gpairs")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Vietnam
# ------------------------------------------------------------------------------

data("Vietnam", package = "vcdExtra")

dim(Vietnam)
str(Vietnam)


car::some(Vietnam)


# frequency form in dataframe to table
( vietnam.tab <- xtabs(Freq ~ sex + year + response, data = Vietnam) )

str(vietnam.tab)



# ------------------------------------------------------------------------------
# Partial association  /  separate model
#   - In 3-way (or larger) table it may be that 2 variables, say A and B, are associated at some levels of the third variable, C, but not at other levels of C.
#     More generally, we may wish to explore whther and how the association among two (or more) variables in a contingency table varies over the levels
#     of the remaining variables.
#   - The term "partial association# refers to the association among some variables within the levels of the other variables
#   - Partial association represents useful "divide and conquer" statistical strategy
# ------------------------------------------------------------------------------


mods.list <- apply(vietnam.tab, "sex", function(x) loglm(~ response * year, data = x))


mods.list



# ----------
mosaic(vietnam.tab["Male",,], shade = TRUE, gp_args = list(interpolate = 1:4), margin = c(right = 1), main = "sex: Male")

mosaic(vietnam.tab["Female",,], shade = TRUE, gp_args = list(interpolate = 1:4), margin = c(right = 1), main = "sex: Female")



# ----------
# This is the same:  decomposed G^2 by separately
# conditional independence:  the response versus year separately for males and females
mosaic(vietnam.tab, expected = ~ response * sex + year * sex, gp_arges = list(interpolate = 1:4), maring = c(right = 1), main="Model: ~ response * sex + year * sex")



