setwd("//media//kswada//MyFiles//R//tea")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS", "gpairs")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  tea
#  - survey of 300 tea drinkers for tea-drinking behaviour (19 questions), the image they have of the product (12 questions) and 4 descriptive quesionts
# ------------------------------------------------------------------------------

tea <- read.table("tea.csv", header = TRUE, sep = ";")

dim(tea)

str(tea)


car::some(tea)



# ------------------------------------------------------------------------------
# Multiple correspondence analysi by mjca()
# ------------------------------------------------------------------------------

tea.mjca <- mjca(tea, lambda = "Burt")


summary(tea.mjca)



# -->
# only 15.2% of the total inertia is accounted for in 2 dimensions.



# ----------
par(mfrow = c(1,1))
plot(tea.mjca)



# -->
# Difficult to understand ....


