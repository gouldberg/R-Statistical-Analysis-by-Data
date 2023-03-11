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
# Princal Loadings
# ------------------------------------------------------------------------------

prinwp1 <- princals(WP6, ordinal = FALSE)



graphics.off()
op <- par(mfrow = c(1,2))

plot(prinwp1, main = "Nominal Princals Loadings")




# ------------------------------------------------------------------------------
# Joint plot with Princal's restricted category quantification
# ------------------------------------------------------------------------------

# convert to homals class and plot
class(prinwp1) <- "homals"


for (i in 1:ncol(WP6)) {
  quants <- prinwp1$quantifications[[i]]
  ind <- apply(quants, 1, function(xx) all(xx == 0))
  quants <- quants[!ind, ]
  rownames(quants) <- rownames(homwp$quantifications[[i]])
  prinwp1$quantifications[[i]] <- quants
}


plot(prinwp1, col.points = colvec, main = "Nominal Princals Joint Plot")


library("colorspace")
colvec <- rainbow_hcl(ncol(WP6), 80)

for (i in 1:ncol(WP6)) {
  xy <- prinwp1$loadings[i,]
  fit <- lm(xy[2] ~ -1 + xy[1])
  abline(fit, col = colvec[i])
}

par(op)



# -->
# Second plot:  Princals category quantifications
#   - Due to the fact that the second dimension is linearly dependent on the first dimension, for each variable the quantifications have to be on a straight line.
#   - The direction of this line is determined by the direction of the loadings vector.
#   - We should not be mistaken and interpret the loadings plot of a nominal Princals solution ina directional way (e.g., "higher" or "larger"), 
#     since we operate on a nominal level.
#   - For variables whose categories are quantified similary (e.g., gay marriage, gay adoption, sexual freedom), the loading vectors point to the same direction.
#     Thus, we get some insight which items are similarly scored.

# First plot:  Princals loadings
#   - In second plot, the lines for gender quotas and affirmative actions are very close to each other, whereas in the first plot (loadings plot),
#     the arrows point into the opposite direction.
#     This is due to the fact that in the bottom-left quadrant of the plot, the quantification for 2 in affirmative action is close to 0 in gender quotas,
#     and gender quota 2 is close to affirmative action 0

