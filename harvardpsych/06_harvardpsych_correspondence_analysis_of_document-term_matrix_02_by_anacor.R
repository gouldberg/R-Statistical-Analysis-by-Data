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




# ------------------------------------------------------------------------------
# Simple correspondence analysis
# ------------------------------------------------------------------------------

library(anacor)

fit_HP <- anacor(HarvardPsych)



# ----------
fit_HP


# -->
# With 2 dimensions, we explain 24.93% of the inertia.  (cumulative proportion of Chi-square decomposition up to Dimension 2 = 24.9)
# This is not a lot, but it is sufficient to get a rough overview of what is going on in terms of research at the department.



# ----------
# The symmetric map of 2D solution
plot(fit_HP, main = "Harvard Psychology Faculty", asp = NULL, xlim = c(-4, 1))

plot(fit_HP, main = "Harvard Psychology Faculty (Zoom)", asp = NULL, xlim = c(0, 1), ylim = c(-1, 1))


# -->
# Since the points are cluttered to the right of the origin, the plot in the bottom panel zooms into the respective area.
# This is symmetric map, so reseacher-to-researcher distances (X^2 distances) are directly interpretable,
# as well as the word-to-word distances.



# ----------
fit_HP$eigen.values
sqrt(fit_HP$eigen.values)


# -->
# The singular values for the first two dimensions are 0.77 and 0.651.
# They do not differ heavily from each other, thus, we can interpret the researcher-to-word associations with reasonable assurance.


plot(fit_HP, main = "Harvard Psychology Faculty (Zoom)", asp = NULL, xlim = c(0, 1), ylim = c(-1, 1))
