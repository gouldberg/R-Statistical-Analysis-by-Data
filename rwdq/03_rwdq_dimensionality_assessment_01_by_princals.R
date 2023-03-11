setwd("//media//kswada//MyFiles//R//rwdq")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  RWDQ (Work Design Questionnaire R Package Authors)
# ------------------------------------------------------------------------------

data("RWDQ", package = "MPsychoR")

str(RWDQ)


car::some(RWDQ)



# ------------------------------------------------------------------------------
# Dimensionality assessment by Princals
# ------------------------------------------------------------------------------

library(Gifi)


# first 2 components explains 76.4% by ndim = 4, so we apply default ndim = 2
# prin_rwdq <- princals(RWDQ, ndim = 4)
prin_rwdq <- princals(RWDQ)



# ----------
summary(prin_rwdq)




# ----------
# loadings plot
par(mfrow = c(1, 1))

plot(prin_rwdq, "loadplot")




# ----------
prin_rwdq$loadings




# -->
# for 1st component, wdq_37 is mostly related
#   - wdq_37:  The tools, procedures, materials, and so forth used to develop R packages are highly specialized in terms of purpose.

# for 2nd component (in negative), wdq_28 and 39 is mostly related
#   - wdq_28:  The work on R packages involves solving problems that have no obvious correct answer.
#   - wdq_39:  The work on R packages requires a depth of expertise.

# for 2nd component (in positive), wdq_33 is mostly related
#   - wdq_33:  The work on R packages requires programming skills.
#   - wdq_25:  The work on R packages requires that I engage in a large amount of thinking.
#   - wdq_27:  The work on R packages requires me to analyze a lot of information
#   - wdq_29:  The work on R packages requires me to be creative.



# ------------------------------------------------------------------------------
# biplot
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))


# expand = 0.5 to shorten the vectors for better representation
plot(prin_rwdq, plot.type = "biplot", main = "Princals Biplot", expand = 0.5, cex.scores = 0.6, col.scores = "gray")

abline(h = 0, v = 0, lty = 2, col = "gray")

