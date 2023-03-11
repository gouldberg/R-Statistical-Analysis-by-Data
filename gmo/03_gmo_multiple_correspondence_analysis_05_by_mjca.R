setwd("//media//kswada//MyFiles//R//gmo")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS", "gpairs")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  gmo
# ------------------------------------------------------------------------------

gmo <- read.table("gmo.csv", header = TRUE, sep = ";", dec = ".")

dim(gmo)

str(gmo)

car::some(gmo)


# ----------
levels(gmo$Position.Al.H)[4] <- levels(gmo$Position.Al.H)[1]

levels(gmo$Position.Culture) <- c("Favourable", "Somewhat Against", "Totally opposed", "Favourable")



# ------------------------------------------------------------------------------
# Multiple correspondence analysi by mjca()
# ------------------------------------------------------------------------------

gmo.mjca <- mjca(gmo[,1:16], lambda = "Burt")


summary(gmo.mjca)



# -->
# 67.6% of the total inertia is accounted for in 2 dimensions.



# ----------
par(mfrow = c(1,1))
plot(gmo.mjca, arrows = c(TRUE, TRUE))



# -->
# Difficult to understand ....


