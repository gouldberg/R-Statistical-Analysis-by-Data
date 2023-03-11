setwd("//media//kswada//MyFiles//R//haireye_color")

packages <- c("dplyr", "datasets", "ca", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  HairEyeColor
#  - frequencies of hair color, eye color, and sex from 592 students in a statistical course (Snee's 1974 sample)
# ------------------------------------------------------------------------------
data("HairEyeColor", package = "datasets")

dim(HairEyeColor)
str(HairEyeColor)

HairEyeColor



# ------------------------------------------------------------------------------
# Multiple Correspondence Analysis (Bivariate MCA):  HairEyeColor
#  - Bivariate MCA is the application of the simple correspondence analysis algorithm to the indicator matrix Z.
#  - This analysis would yield scores for the rows of Z (the cases), usually not of direct interest, and for the columns (the categories of both variables)
#  - The point for any category is the centroid of all the observations with a response in that category, and all observations with the same response pattern coincide.
#
#  - The origin reflects the weighted average of the categories for EACH variable.
#    As the result, category points with low marginal frequencies will be located further away from the origin, while categories with high marginal frequencies will be closer to the origin.
#    For a binary variable, the tow category points will appear on a line through the origin, with distances inversely proportional to their marginal frequencies.
#
#  - We see that the general pattern of the hair color and eye color categories is the same in the analysis of the contingency table
#    and the analysis of the indicator matrix, except that the axes are scaled differently -- the display has been stretched along the second (vertical) dimension.
#    INDEED, the two displays are identical, except for changes in scales along the axes.
# ------------------------------------------------------------------------------
# First, collapse the HairEyeColor data to 2-way table (= male and female)
( haireye <- margin.table(HairEyeColor, 1:2) )


# Generate indicator matrix for 592 samples


# model matrix
( haireye.df <- cbind(as.data.frame(haireye), model.matrix(Freq ~ Hair + Eye, data = as.data.frame(haireye))) )


# model matrix, levels of hair color (Hair1-Hair4) and eye color (Eye1-Eye4)
( haireye.df <- cbind(as.data.frame(haireye), model.matrix(Freq ~ Hair + Eye, data = as.data.frame(haireye), contrasts.arg = list(Hair = diag(4), Eye = diag(4)))[,-1]) )


# Indicator matrix
Z <- vcdExtra::expand.dft(haireye.df)[,-(1:2)]
vnames <- c(levels(haireye.df$Hair), levels(haireye.df$Eye))
colnames(Z) <- vnames
dim(Z)

Z


# Check original contingency table
( N <- t(as.matrix(Z[,1:4])) %*% as.matrix(Z[,5:8]) )



# ---------------------------------------------
Z.ca <- ca::ca(Z)



# ---------------------------------------------
par(mfrow=c(1,2))


# "what" is used to suppress the display of the row points for the cases
res <- plot(Z.ca, what=c("none", "all"), labels = 0, pch = ".", xpd = TRUE)

coords <- data.frame(res$cols)
coords$factor <- rep(c("Hair", "Eye"), each = 4)
coords$levels <- rownames(res$cols)
coords
coords <- coords[ order(coords[,"factor"], coords[,"Dim1"]), ]

cols <- c("blue", "red")
nlev <- c(4,4)
text(coords[,1:2], coords$levels, col=rep(cols, nlev), pos=2, cex=1.2)
points(coords[,1:2], pch=rep(16:17, nlev), col=rep(cols, nlev), cex=1.2)

lines(Dim2 ~ Dim1, data=coords, subset=factor=="Eye",  lty=1, lwd=2, col=cols[1])
lines(Dim2 ~ Dim1, data=coords, subset=factor=="Hair", lty=1, lwd=2, col=cols[2])


# Compare with simple correspondence analysis
plot(haireye.ca)


# --> We see that the general pattern of the hair color and eye color categories is the same in the analysis of the contingency table
# and the analysis of the indicator matrix, except that the axes are scaled differently -- the display has been stretched along the second (vertical) dimension.
# INDEED, the two displays are identical, except for changes in scales along the axes.



# ---------------------------------------------
# major difference between analysis of the contingency table and analysis of the indicator matrix is in the decomposition of principal inertia
# and corresponding chi-square contributions for the dimensions.



# ------------------------------------------------------------------------------
# Multiple Correspondence Analysis (Bivariate MCA)  the Burt matrix:  HairEyeColor
#  - The same solution for the category points as in the analysis of the indicator matrix may be obtained more simply from the so-called Burt matrix
# ------------------------------------------------------------------------------
# Generate Burt matrix
# Diagonal matrices containing the marginal frequencies of the two variables and off-diagonal block contains the contingency table of the two variables
Burt <- t(as.matrix(Z)) %*% as.matrix(Z)
rownames(Burt) <- colnames(Burt) <- vnames
Burt



# ---------------------------------------------
# standard coordinates from an analysi of the Burt matrix are identical to those of Z (indicator matrix)
Burt.ca <- ca::ca(Burt)
plot(Burt.ca)


