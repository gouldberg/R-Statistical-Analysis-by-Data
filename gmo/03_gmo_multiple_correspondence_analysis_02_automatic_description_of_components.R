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
# Multiple Correspondence Analysis by FactoMineR with ventilation
# ------------------------------------------------------------------------------

library(FactoMineR)

graphics.off()
par(mfrow = c(2,2))

res.mca <- MCA(gmo, ncp = 5, quali.sup = 17:21, level.ventil = 0.05)

summary(res.mca)



# ------------------------------------------------------------------------------
# Automatic description of the component
# ------------------------------------------------------------------------------

dimdesc(res.mca)$'Dim 1'$quali



# ----------
dimdesc(res.mca)$'Dim 1'$category


# -->
# Since most variables have two categories, characterisation by category is similar to that calculated from the variables,
# but specifies the direction of the component.



# ----------
# rounding
lapply(dimdesc(res.mca), lapply, round, 4)


lapply(dimdesc(res.mca), lapply, signif, 3)
