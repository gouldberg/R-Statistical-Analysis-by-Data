setwd("//media//kswada//MyFiles//R//tv")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  TV viewing data
# ------------------------------------------------------------------------------
data("TV", package = "vcdExtra")

TV

dim(TV)

str(TV)


TV2 <- margin.table(TV, c(1,3))

TV2



# ------------------------------------------------------------------------------
# Stacking table:  3-way table into 2-way stacked table
# ------------------------------------------------------------------------------

TV.df <- as.data.frame.table(TV)


levels(TV.df$Time) <- rep(c("8", "9", "10"), c(4, 4, 3))


TV3 <- xtabs(Freq ~ Day + Time + Network, TV.df)


TV3



# ------------------------------------------------------------------------------
# Multiple correspondence analysi by mjca() for 3-way table directly
# ------------------------------------------------------------------------------

library(ca)

tv.mca <- mjca(TV3, lambda = "Burt")


summary(tv.mca)



# -->
# only 35.5% of the total inertia is accounted for in 3 dimensions.
# 1st dimension:  largest contributions by "Network:NBC" but almost close is "Day:Thursday"
# 2nd dimension:  largest contributions by "Network:CBS" and second is "Network:ABC"



# ----------
par(mfrow = c(1,1))
plot(tv.mca)





