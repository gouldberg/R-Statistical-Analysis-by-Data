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
# Multiple correspondence analysi by mjca() for 3-way table directly
# ------------------------------------------------------------------------------

library(ca)


viet.mca <- mjca(vietnam.tab, lambda = "Burt")


summary(viet.mca)



# -->
# only 51.3% of the total inertia is accounted for in 2 dimensions.
# 1st dimension:  largest contributions by "sex:Female", "sex:Male", "response C", and "response A"
# 2nd dimension:  largest contributions by "response D"



# ----------
par(mfrow = c(1,1))
plot(viet.mca)





