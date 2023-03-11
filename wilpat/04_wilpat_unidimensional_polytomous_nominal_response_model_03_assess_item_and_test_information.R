setwd("//media//kswada//MyFiles//R//wilpat")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  WilPat
# ------------------------------------------------------------------------------

data("WilPat", package = "MPsychoR")

str(WilPat)



# ----------
wpit15 <- WilPat[,1:15]

names(wpit15)



# ----------
# Eliminate 4 items based on 2-dimensional Princals analysis
elim <- c("Nationalism", "Patriotism", "ChurchAuthority", "Obedience")

ind <- match(elim, colnames(wpit15))

wpitnew <- wpit15[, -ind]




# ------------------------------------------------------------------------------
# Item Information
#   - In many applications, it is of interest in which area of the trait an item is particularly informative.
#     In other words, what is the degree to which an item reduces the uncertainty in estimation of a person's trait value ?
# ------------------------------------------------------------------------------

library(mirt)


# ----------
# plot item information
plot(nrmwp, type = "infotrace", main = "Item Information")



# -->
# From the information curve, we see that "privatization" is highly informative within a narrow range at the center of the trait.
# (average conservative respondents),

# "Lower taxes" is not a very informative item; ultimately no one likes to pay taxes.
# The curve has a little bump located in the liberal respondents' area.



# ----------
lapply(1:ncol(wpitnew), function(x) table(wpitnew[[x]]))

table(wpitnew$Privatization)

table(wpitnew$LowerTaxes)

xtabs(~ Privatization + LowerTaxes, data = wpitnew)




# ------------------------------------------------------------------------------
# Test Information
#   - These item information curves can be aggregated. This leads to the concept of test information
# ------------------------------------------------------------------------------

library(mirt)


# ----------
# plot test information
plot(nrmwp, type = "info", main = "Test Information")


# -->
# This plot tells us in which trait area our entire scale is informative and thus able to assess a person's location on the conservatism trait with good precision.




