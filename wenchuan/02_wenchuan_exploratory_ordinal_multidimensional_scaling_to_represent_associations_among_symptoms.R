setwd("//media//kswada//MyFiles//R//wenchuan")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Wenchuan
# ------------------------------------------------------------------------------

data("Wenchuan", package = "MPsychoR")

str(Wenchuan)



# ------------------------------------------------------------------------------
# Exploratory MDS
#   - Using MDS as an exploratory technique has a long tradition in the psychometric literature.
#     An optimization breakthrough was achieved by De Leeuw (1997) who proposed a numerical approach (majorization) to solve the MDS problem.
#     This idea led to the SMACOF (Scaling by Majorizing Complicated Function) framework.
#     Because of its flexibility, SMACOF is nowadays considered as the state-of-the-art approach for fitting MDS models.
# ------------------------------------------------------------------------------

# transpose and compute Euclidean distances
Wdelta <- dist(t(Wenchuan))

Wdelta



# ----------
library(smacof)

fit.wenchuan1 <- mds(Wdelta, type = "ordinal")

fit.wenchuan1


# -->
# The MDS fit results in a stress-1 value = 0.133, whether the stress value in this example suggests a good model fit is not trivial to answer.



# ----------
summary(fit.wenchuan1)

plot(fit.wenchuan1, main = "Wenchuan MDS")



# -->
# The plot shows how the PTSD symptoms are related to each other.
# We see that "avoiding thinking about or talking about a stressful experience from the past or avoiding having feelings related to it" (avoidth) and
# "avoiding activities or situations because they reminded you of a stressful experience from the past" (avoidact) 
# are virtually on the same spot.

# We also see that "feeling very upset when something reminded you of a stressful experience from the past ?" (upset)
# and "feeling emotionally numb or being unable to have loving feelings for those close to you" (numb) 
# are the farthest points on the first dimension,
# whereas "trouble remembering important parts of a stressfule experience from the past" (amnesia) and
# "feeling irritable or having angry outbursts" (anger) are the extreme points in the second dimension.
# These pairs of symptoms are pretty much unrelated to each other.

# Note that an MDS configuration can be rotated arbitrarily, if it helps for interpretation.
# There is no natural law that guarantees that the dimensions are always interpretable.



