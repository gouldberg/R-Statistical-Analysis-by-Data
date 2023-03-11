setwd("//media//kswada//MyFiles//R//suicide")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Suicide rates in Germany
# ------------------------------------------------------------------------------
data("Suicide", package = "vcd")

data <- Suicide

data


# Interactive coding of sex and age.group
data <- within(data, {
  age_sex <- paste(age.group, toupper(substr(sex, 1, 1)))
})


car::some(data)


( tab <- xtabs(Freq ~ age_sex + method2, data = data) )



# ------------------------------------------------------------------------------
# Principal inertia
# ------------------------------------------------------------------------------

# By default, ca() produces a 2-dimensional solution
( suicide.ca <- ca::ca(tab) )


summary(suicide.ca)



# -->
# 92.6% of the Pearson chi-square for association (= Principal inertias) is accounted for by 2 dimensions,



# ------------------------------------------------------------------------------
# Visualize pattern of associations by plotting CA scores in 2D
# --> 2D CA solution for the stacked [AgeSex][Method] table of the suicide data
# ------------------------------------------------------------------------------
# Standard coordinates are transformed internally within the plot function according to the map argument: default to map = "symmetric",
# giving principal coordinates

# asp = 1:  2 axes are scaled so that the number of data units per inch are the same for both the horizontal and vertical axes.
graphics.off();  par(mfrow=c(1,1), asp = 1)
res <- plot(suicide.ca)

res



# -->
# It is important for interpretation of distances between points (and angles between vectors) for the axes to be equated.
# (this is symmetic map, so the distances between row points and distances between column points are meaningful)

# Dimension 1:
#  - separates males (right) and females (left), indicating a large difference between suicide profiles of males and females with respect to methods of suicide.
# Dimension 2:
#  - mostly ordered by age with younger groups at the bottom and older  groups at the top.
#  - Note also that the positions of the age groups are roughly parallel for the two sexes.
#    Such a pattern indicates that sex and age do not interact in this analysis

# The relation between the age-sex groups and methods of suicide can be approximately interpreted in terms of similar distance and direction from the origin,
# which represents the marginal row and column profiles.

# Young males are more likely to commit suicide by gas or a gun, older males by hanging,
# while young females are more likely to ingest some toxic agent and older females by jumping or drowning

