setwd("//media//kswada//MyFiles//R//gopdtm")

packages <- c("dplyr", "smacof")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  GOPdtm
# ------------------------------------------------------------------------------

data(GOPdtm, package = "smacof")


str(GOPdtm)


car::some(GOPdtm)



# ------------------------------------------------------------------------------
# Power gravity model for co-occurrences
# ------------------------------------------------------------------------------

# Convert document term matrix to dissimilarities using a gravity model based on co-occurrences
gravD05 <- gravity(GOPdtm, lambda = 0.5)
gravD10 <- gravity(GOPdtm, lambda = 1)
gravD20 <- gravity(GOPdtm, lambda = 2)
gravD50 <- gravity(GOPdtm, lambda = 5)


gravD20$gravdiss


gravD20$co.occ



# ----------
res05 <- mds(gravD05$gravdiss)
res10 <- mds(gravD10$gravdiss)
res20 <- mds(gravD20$gravdiss)
res50 <- mds(gravD50$gravdiss)


# NA's were blanked out when fitting the model
res20$weightmat



# ----------
res05$stress
res10$stress
res20$stress
res50$stress



# ----------
graphics.off()

par(mfrow = c(2,2))
plot(res05, main = "lambda = 0.5")
plot(res10, main = "lambda = 1")
plot(res20, main = "lambda = 2")
plot(res50, main = "lambda = 5")



# Mair et al. (2014) used lambda = 2 for the data and remark that
# "there is a trade^off between the structure determined by lambda and the goodness of ift as quantified by the Stress value:
# The  mode structure we create, the higher the Stress value"
# This is so because heavily weighting large dissimilarities, for example, makes it generally easier to find a low-Stress solution,
# because then most of the smaller dissimilarities have essentially zero weights in the Stress measure.
# With huge lambda's, only very few dissimilarities matter.


