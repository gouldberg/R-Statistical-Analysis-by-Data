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




# ------------------------------------------------------------------------------
# Combined Homals-Princals strategies
#  - For nominal transformations we are mostly interested in unrestricted category quantifications,
#    whereas for metric transformations (e.g., linear or monotone spline), we want to obtain loadings based on a restricted solution.
#    For ordinal transformations both an unrestricted and a restricted solution can be considered.
#    It depends on whether we want to put more emphasis on the category quantifications or the loadings.
#  - This gives a more intuitive output by avoiding the seemingly awakward second-dimension transformations for the non-nominal variables.
#  - we relax linear restriction using copies in Princals, "copies" produce many copies for each variables
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
# Transform mixed data by Princals
# ------------------------------------------------------------------------------

# We need to specify a copies vector for the nominal variables (i.e., six items, country and gender)

( copvec <- c(rep(2, 7), 1, 1, 2, 1) )

prinmix <- princals(WPmixed, knots = knotlist, ordinal = ordvec, degrees = degvec, copies = copvec)




# ------------------------------------------------------------------------------
# transformation plot
#  - The black lines show the transformation on the first dimension
#  - The red lines the transformations on the second dimension
# ------------------------------------------------------------------------------

# Legalized marijuana item:  we get a transformation on each dimension
# LibCons:  original categories are merged into 1 vs. higher by the ordinal transformation.
# LeftRight:  we see the spline pattern (categories 1-7 get the same transformed scores)
# Age: taken as numeric and linearly transformed.

op <- par(mfrow = c(3,2), mar = c(4,4,3,2))

# marijuana
plotvars <- as.matrix(prinmix$datanum[, 6])
xlabels <- as.data.frame(prinmix$data[, 6])
x1 <- as.matrix(prinmix$datanum[, 6])
y1 <- as.matrix(prinmix$transform[, c(11, 12)])
xy <- cbind(x1, y1)
ord <- order(xy[, 1])
sfun0 <- stepfun(xy[ord, 1][-1], xy[ord, 2], f = 0)
plot(sfun0, xlab = "original", ylab = "transformed", main = "LegalizedMarijuana", xaxt = "n", col = "black", 
     do.points = FALSE, verticals = FALSE, ylim = range(xy[,2:3]))
axis(1, labels = unique(xlabels[,1]), at = unique(x1))
sfun1 <- stepfun(xy[ord, 1][-1], xy[ord, 3], f = 0)
plot(sfun1, add = TRUE, col = "red", do.points = FALSE, verticals = FALSE)


# country
plotvars <- as.matrix(prinmix$datanum[, 7])
xlabels <- as.data.frame(prinmix$data[, 7])
x1 <- as.matrix(prinmix$datanum[, 7])
y1 <- as.matrix(prinmix$transform[, 13])
xy <- cbind(x1, y1)
ord <- order(xy[, 1])
sfun0 <- stepfun(xy[ord, 1][-1], xy[ord, 2], f = 0)
plot(sfun0, xlab = "original", ylab = "transformed", main = "Country", xaxt = "n", col = "black", 
     do.points = FALSE, verticals = FALSE)
axis(1, labels = unique(xlabels[,1]), at = unique(x1))


# libcons
plotvars <- as.matrix(prinmix$datanum[, 8])
xlabels <- as.data.frame(prinmix$data[, 8])
x1 <- as.matrix(prinmix$datanum[, 8])
y1 <- as.matrix(prinmix$transform[, 14])
xy <- cbind(x1, y1)
ord <- order(xy[, 1])
sfun0 <- stepfun(xy[ord, 1][-1], xy[ord, 2], f = 0)
plot(sfun0, xlab = "original", ylab = "transformed", main = "LibCons", xaxt = "n", col = "black", 
     do.points = FALSE, verticals = FALSE)
axis(1, labels = unique(xlabels[,1]), at = unique(x1))


# left-right
plotvars <- as.matrix(prinmix$datanum[, 9])
xlabels <- as.data.frame(prinmix$data[, 9])
x1 <- as.matrix(prinmix$datanum[, 9])
y1 <- as.matrix(prinmix$transform[, 15])
xy <- cbind(x1, y1)
ord <- order(xy[, 1])
plot(xy[ord, 1], xy[ord, 2], type = "l", xlab = "original", ylab = "transformed", main = "LeftRight", 
     xaxt = "n", col = "black")
axis(1, labels = unique(xlabels[, 1]), at = unique(x1))


# Gender
plotvars <- as.matrix(prinmix$datanum[, 10])
xlabels <- as.data.frame(prinmix$data[, 10])
x1 <- as.matrix(prinmix$datanum[, 10])
y1 <- as.matrix(prinmix$transform[, 16])
xy <- cbind(x1, y1)
ord <- order(xy[, 1])
sfun0 <- stepfun(xy[ord, 1][-1], xy[ord, 2], f = 0)
plot(sfun0, xlab = "original", ylab = "transformed", main = "Gender", xaxt = "n", col = "black", 
     do.points = FALSE, verticals = FALSE)
axis(1, labels = unique(xlabels[,1]), at = unique(x1))


# Age
plotvars <- as.matrix(prinmix$datanum[, 11])
xlabels <- as.data.frame(prinmix$data[, 11])
x1 <- as.matrix(prinmix$datanum[, 11])
y1 <- as.matrix(prinmix$transform[, 17])
xy <- cbind(x1, y1)
ord <- order(xy[, 1])
plot(xy[ord, 1], xy[ord, 2], type = "l", xlab = "original", ylab = "transformed", main = "Age", xaxt = "n", 
     col = "black")
axis(1, labels = unique(xlabels[, 1]), at = unique(x1))
par(op)



# ------------------------------------------------------------------------------
# For comparison, check transformations by Homals
# ------------------------------------------------------------------------------

hommix <- homals(WPmixed, knots = knotlist, ordinal = ordvec, degrees = degvec)


par(mfrow = c(3,2))
plot(hommix, plot.type = "transplot", var.subset = 6:11)


# -->
# LibCons and LeftRight are major differences



# ------------------------------------------------------------------------------
# Joint Plot
#  - with category quantifications for the variables transformed using a linear function, spline, and step function
#  - For LibCons, in addition to the loading, we also plot the category quantifications (gray labels) which are on a straight line since we did not make a copy.
#  - All nominal quantifications were unrestricted and therefore free to vary in the 2D space.
# ------------------------------------------------------------------------------


# ----------
# Loadings from prinmix:  "Libcons", "LeftRight" and "Age" 
# loadings are divided by 10 for better mapping into the category quantificaiton space

loads <- prinmix$loadings[c(14, 15, 17), ]/10

rownames(loads) <-  c("LibCons", "LeftRight", "Age")



# ----------
# Quantification
quants <- prinmix$quantifications[c(1:7, 10)]

for (i in 1:6) rownames(quants[[i]]) <- c(0,1,2)

rownames(quants[[7]]) <- c("India", "Hungary")
rownames(quants[[8]]) <- c("Female", "Male")

quants



# ----------
par(mfrow = c(1,1))

plot(loads, type = "p", pch = ".", xlab = "Dimension 2", ylab = "Dimension 2", main = "Princals Joint Plot", 
     col = "gray", asp = 1, ylim = c(-0.04, 0.06), xlim = c(-0.07, 0.05))
abline(h = 0, v = 0, col = "gray", lty = 2)
arrows(0, 0, loads[1, 1], loads[1, 2], length = 0.08, col = 1)
arrows(0, 0, loads[2, 1], loads[2, 2], length = 0.08, col = 1)
arrows(0, 0, loads[3, 1], loads[3, 2], length = 0.08, col = 1)
text(loads, labels = rownames(loads), pos = c(3,1,2), cex = 0.8, col = 1)



# ----------
colvec <- colorspace::rainbow_hcl(length(quants), 80)

for (i in 1:length(quants)) {
  points(quants[[i]], pch = 20, col = colvec[i], cex = 0.7)
  labs <- paste(names(quants)[i], rownames(quants[[i]]), sep = ".")
  text(quants[[i]], labels = labs, pch = 20, col = colvec[i], cex = 0.7, pos = 3)
}



# ----------
lcquants <- prinmix$quantifications[[8]]

rownames(lcquants) <- 1:4
points(lcquants, pch = 20, col = "gray", cex = 0.7)
labs <- paste("LibCons", rownames(lcquants), sep = ".")
text(lcquants, labels = labs, pch = 20, col = "gray", cex = 0.7, pos = c(4,2,3,4))


