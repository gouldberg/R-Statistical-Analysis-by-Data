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
# Confidence ellipses around categories of a categorical variable
# (i.e., around the barycentre of the individuals carrying that category)
# to assess whether two categories are significantly different or not
# ------------------------------------------------------------------------------

plotellipses(res.mca, keepvar = c("Position.Culture", "Position.Al.A", "Profession", "Political.Party"))



# ------------------------------------------------------------------------------
# Confidence ellipses around categories of a single categorical variable
# ------------------------------------------------------------------------------

# keepvar = 21: Policital.Party
plotellipses(res.mca, keepvar = 21, label = "none")

