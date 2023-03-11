setwd("//media//kswada//MyFiles//R//wilpat")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  WilPat
# ------------------------------------------------------------------------------

data("WilPat", package = "MPsychoR")

str(WilPat)



# ----------
# Let us pick six items: gay marriage, sexual freedom, gay adoption, gender quotas, affirmative action, and legalized marijuana
# and country variable (Hungary, the USA, and India)
WP6 <- WilPat[, c(32, 38, 41, 44, 45, 46, 47)]




# ------------------------------------------------------------------------------
# Multiple Correspondence Analysis Using Homals
#   - French Approach to multiple CA:  use a singular value decomposition on either the Burt matrix or the indicator matrix
#   - Dutch Approach (i.e., Homals) solves the multiple CA problem numerically, which offers a great amount of flexibility
# ------------------------------------------------------------------------------

library(Gifi)

homwp <- homals(WP6)

summary(homwp)



# -->
# variance accounted for 1st 2 dimensions are only 33.71%



# ------------------------------------------------------------------------------
# Check cateogry quantifications  (MCA's coordinate)
# ------------------------------------------------------------------------------

plot(homwp, plot.type = "transplot", var.subset = 1:4)


homwp$quantifications$GayMarriage




# ------------------------------------------------------------------------------
# Symmetric map (joint plot)
# ------------------------------------------------------------------------------
# In the Princals applications, we are mostly interested in plotting the lodings.
# In Homals the most important plot is based on category quantifications, that is, a symmetric CA map which Gifi calls joint plot.
# In this plot we are interested in interpreting associations among the single item categories, associations among countries, and how the countries
# are associated with the item categories.
# Note that in Homals the same issues apply as in single and multiple CA when it comes to interpreting distances between categories of different items

par(mfrow = c(1,1))

plot(homwp)



# -->
# Dimension 1 discriminates between 0 and 1 responses, whereas the second dimension is mostly determined by the "don't know" answers.
# Participants from India tend to disapprove on all items.
# US participants tend to approve, and HUngarian participants tend to respond "don't know".



# ----------
# plot separately by variable
par(mfrow = c(2,2))
plot(homwp, var.subset = 1)
plot(homwp, var.subset = 2)
plot(homwp, var.subset = 3)
plot(homwp, var.subset = 4)


