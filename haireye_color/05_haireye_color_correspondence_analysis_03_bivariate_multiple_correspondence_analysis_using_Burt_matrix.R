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
# Bivariate Multiple Correspondence Analysis by Burt matrix
# ------------------------------------------------------------------------------

Burt <- t(as.matrix(Z)) %*% as.matrix(Z)
rownames(Burt) <- colnames(Burt) <- vnames
Burt



## ----Burt2, fig.keep='none'----------------------------------------------
Burt.ca <- ca(Burt)
plot(Burt.ca)




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
