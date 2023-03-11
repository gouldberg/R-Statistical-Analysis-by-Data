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


# In addition, include 4 more variables:
#   - self-reported liberal-consevative item (re-categorized into 4 categories)  (we will take as ordinal)
#   - self-reported political lef-right identification item on a 10-point scale  (we use a spline transformation since there are many categories)
#   - gender (nominal) and age (linear)
WPmixed <- WilPat[, c(32, 38, 41, 44, 45, 46, 47:51)]

names(WPmixed)




# ------------------------------------------------------------------------------
# Set up the knots and degrees specification for Homals
# ------------------------------------------------------------------------------

library(Gifi)

table(WPmixed$LibCons)


WPmixed$LibCons <- cut(WPmixed$LibCons, breaks = c(0, 2, 4, 6, 9), labels = 1:4)

WPmixed <- na.omit(WPmixed)



# ----------
# Q:  knots at the quantiles
# R:  equally spaced knots
# E:  interior knots
# D:  knots at the data points

# item knots (data)
itknots <- knotsGifi(WPmixed[, 1:6], "D")

# country knots (data)
cknots <- knotsGifi(WPmixed[, 7], "D")

# lib-cons knots (data)
lcknots <- knotsGifi(WPmixed[, 8], "D")

# left-right (terciles)
lrknots <- knotsGifi(WPmixed[, 9], "Q", n = 2)

# gender knots (data)
genknots <- knotsGifi(WPmixed[, 10], "D")

# age knots (empty)
ageknots <- knotsGifi(WPmixed[, 11], "E")



# ----------
( knotlist <- c(itknots, cknots, lcknots, lrknots, genknots, ageknots) )



# ----------
# specify which variables should be ordianlly restricted
# For all the nominal variables, the element is set as FALSE
( ordvec <- c(rep(FALSE, 6), FALSE, TRUE, TRUE, FALSE, TRUE) )



# The degrees for the nominal variables are set to -1, as required by the homals function
# For the left-right variable, we define a polynomial degree of 2.
( degvec <- c(rep(-1, 7), 1, 2, -1, 1) )




# ------------------------------------------------------------------------------
# Transform mixed data by Homals
# ------------------------------------------------------------------------------

hommix <- homals(WPmixed, knots = knotlist, ordinal = ordvec, degrees = degvec)

hommix

summary(hommix)




# ------------------------------------------------------------------------------
# Check category quantification
# ------------------------------------------------------------------------------

par(mfrow = c(2,2))

plot(hommix, plot.type = "transplot", var.subset = 6:11)



# ----------
# Transformed value
head(hommix$transform$LeftRight)

head(hommix$transform$GayMarriage)

head(hommix$transform$Gender)

head(hommix$transform$Country)



# -->
# We see that some variables are transformed on two dimensions.
# The black lines reflect the transofrmations on the first dimension and the red lines the transformations on the second dimension.

# If we look at the liberal-conservative rating (ordinal) and lef-right rating (spline),
# we see that our transformation restriction only holds for the first dimension.

# For the variables with 2 categories only (country and gender), and the ones with a linear restriction (age),
# We only get transformed scores on the first dimension.



# ------------------------------------------------------------------------------
# Symmetric map (joint plot)
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))

plot(hommix)



# ----------
# plot separately by variable
par(mfrow = c(2,2))
plot(hommix, var.subset = 7)
plot(hommix, var.subset = 8)
plot(hommix, var.subset = 9)
plot(hommix, var.subset = 10)




