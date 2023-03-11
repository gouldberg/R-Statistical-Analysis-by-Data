setwd("//media//kswada//MyFiles//R//orange")

packages <- c("dplyr", "MPsychoR", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  orange
# ------------------------------------------------------------------------------

orange <- read.table("orange.csv", header=TRUE, sep=";", dec=".", row.names=1)

str(orange)

dim(orange)


orange




# ------------------------------------------------------------------------------
# Principal Component Analysis Biplots by Princals
#   - By Princals, we do not have to worry about any standardization issues since the Gifi framework performs optimal scaling anyway
#     and the scores are normalized.
# ------------------------------------------------------------------------------

library(Gifi)


prc <- princals(orange[, 1:7])  

summary(prc)



# ----------
par(mfrow = c(1,1))

# expand = 0.5 to shorten the vectors for better representation
plot(prc, plot.type = "biplot", main = "Princals Biplot", expand = 0.5, cex.scores = 0.6, col.scores = "gray")
abline(h = 0, v = 0, lty = 2, col = "gray")



# ----------
par(mar = c(1,1,1,1))
plot(prc, plot.type = "transplot")
