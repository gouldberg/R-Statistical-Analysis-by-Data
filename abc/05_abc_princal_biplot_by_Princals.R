setwd("//media//kswada//MyFiles//R//abc")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  ABC
# ------------------------------------------------------------------------------

data("ABC", package = "MPsychoR")

str(ABC)

dim(ABC)

car::some(ABC)



# ------------------------------------------------------------------------------
# Principal Component Analysis Biplots by Princals
#   - By Princals, we do not have to worry about any standardization issues since the Gifi framework performs optimal scaling anyway
#     and the scores are normalized.
# ------------------------------------------------------------------------------

library(Gifi)


prc <- princals(ABC[, 6:11])  

summary(prc)



# ----------
par(mfrow = c(1,1))


# expand = 0.7 to shorten the vectors for better representation
plot(prc, plot.type = "biplot", main = "Princals Biplot", expand = 0.7, cex.scores = 0.6, col.scores = "gray")
abline(h = 0, v = 0, lty = 2, col = "gray")




# ----------
par(mar = c(1,1,1,1))
plot(prc, plot.type = "transplot")

