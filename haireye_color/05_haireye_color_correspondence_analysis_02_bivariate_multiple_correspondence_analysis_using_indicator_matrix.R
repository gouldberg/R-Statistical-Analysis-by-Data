setwd("//media//kswada//MyFiles//R//haireye_color")

packages <- c("dplyr", "datasets", "vcd", "vcdExtra", "MASS", "ca")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  HairEyeColor
# ------------------------------------------------------------------------------
data("HairEyeColor", package = "datasets")

data <- HairEyeColor

data


# collapse table to 2-way table (Hair(1) and Eye(2))
( haireye <- margin.table(data, 1:2) )



# ------------------------------------------------------------------------------
# Create indicator matrix Z from frequency table
# ------------------------------------------------------------------------------
haireye.df <- cbind(
  as.data.frame(haireye),
  model.matrix(Freq ~ Hair + Eye, data = haireye, contrasts.arg = list(Hair = diag(4), Eye = diag(4)))[,-1]
)


haireye.df


Z <- expand.dft(haireye.df)[,-(1:2)]
vnames <- c(levels(haireye.df$Hair), levels(haireye.df$Eye))
colnames(Z) <- vnames
dim(Z)


head(Z)



# ----------
# note: if the indicator matrix Z is partitioned as Z = [Z1, Z2],
# corresponding to the 2 sets of categories, then the contingency table is given by t(Z1) %*% Z2
t(as.matrix(Z[,1:4])) %*% as.matrix(Z[,5:8])



# ------------------------------------------------------------------------------
# Bivariate Multiple Correspondence Analysis of the indicator matrix Z
#
#  - MCA does provide a useful graphic portrayal of the bivariate relations among any number of categorical variables and has close relations to mosaic matrix.
#
#  - MCA can be described as the application of the simple correspondence analysis algorithm to the indicator matrix Z.
#    This analysis would yield scores for the rows of Z (the cases), usually not of direct interest, and for the columns (the categories of both variables).
#    As in simple CA, each row point is the weighted average of the scores for the column categories,
#    and each column point is the weighted average of the scores for the row observations.
#
#  - Consequently, the point for any category is the centroid of all the observations with a response in that category,
#    and all observations with the same response pattern coincide.
#
#  - As well, the origin reflects the weighted average of the categories for each variable.
#  - As a result, category points with low marginal frequencies will be located further away from the origin, while categories with high marginal frequencies
#    will be closer to the origin.
#
#  - For a binary variable, the 2 category points will appear on a line through the origin, with distances inversely proportional to their marginal frequencies
#
# ------------------------------------------------------------------------------

Z.ca <- ca(Z)


Z.ca



# ----------
# the argument "what" is used to suppress the display of the row points for the cases
graphics.off();  par(mfrow=c(1,1));
res <- plot(Z.ca, what=c("none", "all"), labels = 0, pch = ".", xpd = TRUE)


# extract factor names and levels
coords <- data.frame(res$cols)
coords$factor <- rep(c("Hair", "Eye"), each = 4)
coords$levels <- rownames(res$cols)
coords


# sort by Dim 1
coords <- coords[ order(coords[,"factor"], coords[,"Dim1"]), ]
cols <- c("blue", "red")
nlev <- c(4,4)
text(coords[,1:2], coords$levels, col=rep(cols, nlev), pos=2, cex=1.2)
points(coords[,1:2], pch=rep(16:17, nlev), col=rep(cols, nlev), cex=1.2)

lines(Dim2 ~ Dim1, data=coords, subset=factor=="Eye",  lty=1, lwd=2, col=cols[1])
lines(Dim2 ~ Dim1, data=coords, subset=factor=="Hair", lty=1, lwd=2, col=cols[2])



# -->
# Except that the axes are scaled differently -- the display has been stretched along the second (vertical) dimension,
# general pattern is same with simple correspondence analysis

# The interpretation is the same:
#  - Dimension 1: reflects a dark-light ordering of both hair and eye colors.
#  - Dimension 2: reflects something that largely distinguishes red hair and green eyes from the other categories


# -->
# Aside from the largely cosmetic difference in relative scaling of the axes, a major difference between analysis of the contingency table
# and analysis of the indicator matrix is in the decomposition of principal inertia and corresponding X^2 contributions for the dimensions.
# The plot axes indicate 24.3% and 19.2% for the contributions for the dimensions.



# ------------------------------------------------------------------------------
# The same solution for the category points in the analysis of the indicator matrix may be obtained
# more simply from the so-called Burt matrix
#
#  - Burt Matrix (B) = t(Z) %*% Z = [N1, N, t(N), N2]
#    where N1 and N2 are diagonal matrices containing the marginal frequencies of the two variables (the column sums of Z1 and Z2)
#    N, the contingency table of the 2 variables, appears in the off-diagonal block
# ------------------------------------------------------------------------------
Burt <- t(as.matrix(Z)) %*% as.matrix(Z)
rownames(Burt) <- colnames(Burt) <- vnames
Burt



# The standard coordinates from an analysis of the Burt matrix B are identical to those of Z.
# However, the singular values of B are the squares of those of Z.
# Then, the following code, using the Burt matrix produces the same display of the category points for hair color and eye color

Burt.ca <- ca(Burt)
plot(Burt.ca)

