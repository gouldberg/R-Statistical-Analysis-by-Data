setwd("//media//kswada//MyFiles//R//accident")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS", "gpairs")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Accident
# ------------------------------------------------------------------------------

data("Accident", package = "vcdExtra")

dim(Accident)

str(Accident)


car::some(Accident)


# frequency form in dataframe to table
( acci.tab <- xtabs(Freq ~ age + result + mode + gender, data = Accident) )



# ------------------------------------------------------------------------------
# Multiple correspondence analysi by mjca() for 4-way table directly
# ------------------------------------------------------------------------------

library(ca)


acci.mca <- mjca(acci.tab, lambda = "Burt")


summary(acci.mca)



# -->
# only 40.9% of the total inertia is accounted for in 2 dimensions.
# 1st dimension:  largest contributions by "mode:Motorcycle", "mode:Pedestrian"
# 2nd dimension:  largest contributions by "age:10-19", "mod:Bycycle"



# ----------
par(mfrow = c(1,1))
plot(acci.mca, arrows = c(TRUE, TRUE))





