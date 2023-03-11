setwd("//media//kswada//MyFiles//R//harvardpsych")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  HarvardPsych
# ------------------------------------------------------------------------------

data("HarvardPsych", package = "MPsychoR")

str(HarvardPsych)

dim(HarvardPsych)


# researchers in rows, words in columns  (29 * 43)
head(HarvardPsych)



# ----------
rownames(HarvardPsych)

colnames(HarvardPsych)




# ------------------------------------------------------------------------------
# Principal Component Analysis Biplots by Princals
#   - By Princals, we do not have to worry about any standardization issues since the Gifi framework performs optimal scaling anyway
#     and the scores are normalized.
# ------------------------------------------------------------------------------

library(Gifi)


prc <- princals(HarvardPsych)  

summary(prc)




# ----------
# object scores

prc$objectscores




# ----------
par(mfrow = c(1,1))

# expand = 0.5 to shorten the vectors for better representation
plot(prc, plot.type = "biplot", main = "Princals Biplot", expand = 0.5, cex.scores = 0.6, col.scores = "gray")
abline(h = 0, v = 0, lty = 2, col = "gray")


# -->
# Now Schacter is represented much higher in D1 (compared to PC biplot)




# ----------
par(mar = c(1,1,1,1))
plot(prc, plot.type = "transplot")



# -->
# Note that "memory" and "language" transplot  (really different)





