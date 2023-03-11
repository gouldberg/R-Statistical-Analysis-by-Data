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
# Unidimensional Dichotomous IRT Models:  Nominal Response Model
#   - This model is more general than the Graded Response Model (GRM) and Generalized Partial Credit Model (GPCM)
#     in the sense that each item-category gets its own individual discrimination parameter, in addition to the item-category location.
#   - The probability that person v scores category h on item i:
#     P(X(vi) = h) = exp( alpha(ih) * (theta(v) - beta(ih)) ) / { 1 + exp( alpha(ih) * (theta(v) - beta(ih)) ) }
#     alpha(ih): item-category discrimination parameter, implying that, neither within nor across items,
#     the item-category characteristic curves have to be parallel.
#
#   - Item-category discriminations can vary drastically within and across items, as in this example.
#     The Nominal Response Model gives highly detailed insight into the item-category/person behaviour.
# ------------------------------------------------------------------------------

library(mirt)


nrmwp <- mirt(wpitnew, 1, itemtype = "nominal")

nrmwp



# ----------
ip1 <- itemplot(nrmwp, 1, main = colnames(wpitnew)[1], auto.key = list(text = c("disapprove", "approve", "don't know"), cex = 0.7))

ip2 <- itemplot(nrmwp, 3, main = colnames(wpitnew)[3], auto.key = list(text = c("disapprove", "approve", "don't know"), cex = 0.7))

ip3 <- itemplot(nrmwp, 4, main = colnames(wpitnew)[4], auto.key = list(text = c("disapprove", "approve", "don't know"), cex = 0.7))

ip4 <- itemplot(nrmwp, 10, main = colnames(wpitnew)[10], auto.key = list(text = c("disapprove", "approve", "don't know"), cex = 0.7))

ip5 <- itemplot(nrmwp, 2, main = colnames(wpitnew)[2], auto.key = list(text = c("disapprove", "approve", "don't know"), cex = 0.7))

ip6 <- itemplot(nrmwp, 6, main = colnames(wpitnew)[6], auto.key = list(text = c("disapprove", "approve", "don't know"), cex = 0.7))

ip7 <- itemplot(nrmwp, 9, main = colnames(wpitnew)[9], auto.key = list(text = c("disapprove", "approve", "don't know"), cex = 0.7))



# ----------
print(ip1, split = c(1, 1, 2, 2), more = TRUE)

print(ip2, split = c(2, 1, 2, 2), more = TRUE)

print(ip3, split = c(1, 2, 2, 2), more = TRUE)

print(ip4, split = c(2, 2, 2, 2), more = FALSE)

print(ip5)

print(ip6)

print(ip7)



# -->
# Genetically modified foods are only approved by very conservative people.
# The remaining persons are most likely against it or say "don't know".

# For the capitalism, free market, and lower taxes itesm, the probability for scoring 0 ("disapprove") is low throughout the trait.
# Lower taxes are approved over a wide range of the continuum: very liberal persons most likely say "don't know".


