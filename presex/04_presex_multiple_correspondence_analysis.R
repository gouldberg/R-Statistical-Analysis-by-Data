setwd("//media//kswada//MyFiles//R//presex")

packages <- c("dplyr", "datasets", "vcd", "vcdExtra", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  PreSex
# ------------------------------------------------------------------------------
data("PreSex", package = "vcd")

str(PreSex)

dim(PreSex)


PreSex



# ----------
# Reorder variables and table
#  - FROM:  MaritalStatus, ExtramaritalSex, PremaritalSex, Gender
#  - TO:  Gender, PremaritalSex, ExtramaritalSex, MaritalStatus
PreSex <- aperm(PreSex, 4:1)




# ------------------------------------------------------------------------------
# Multivariate Multiple Correspondence Analysis
#
#  - There is a close connection between MCA and the bivariate mosaix matrix. The mosaic matrix displays the residuals from independence for each pair of variables,
#   and thus provides a visual representation of the Burt matrix.
#   The total amount of shading in all the individual mosaics portrays the total pairwise associations decomposed by MCA.
#
#  - For interpretation of MCA plots, we note the following relations.
#     - The inertia contributed by a given variable increases with the number of response categories.
#     - The centroid of the categories for each discrete variable is at the origin of the display.
#     - For a particular variable, the inertia contributed by a given category increases as the marginal frequency in that category decreases.
#       Low frequency points therefore appear further from the origin.
#     - The category points for a binary variable lie on a line through the origin. The distance of each point to the origin is inversely related to the marginal frequency.
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Analyze Burt matrix
# ------------------------------------------------------------------------------
PreSex



# ----------
# ca::mjca() typically takes a data frame in case form containing the factor variables, but converts a table to this form.
# This example analyzes the Burt matrix calculated from the PreSex data, specified as lambda = "Burt"
( presex.mca <- ca::mjca(PreSex, lambda = "Burt") )

summary(presex.mca)


# -->
# 77.6% of the total inertia is accounted for in 2 dimensions.



# ----------
# plot --> this is not very flexible
graphics.off();  par(mfrow=c(1,1));
plot(presex.mca)




# ------------------------------------------------------------------------------
# customized display
# ------------------------------------------------------------------------------

res <- plot(presex.mca, labels = 0, pch = ".", cex.lab = 1.2)
coords <- data.frame(res$cols, presex.mca$factors)
nlev <- presex.mca$levels.n
fact <- unique(as.character(coords$factor))

cols <- c("blue", "red", "brown", "black")
points(coords[,1:2], pch=rep(16:19, nlev), col=rep(cols, nlev), cex=1.2)
text(coords[,1:2], label=coords$level, col=rep(cols, nlev), pos=3, cex=1.2, xpd=TRUE)
lwd <- c(2, 2, 2, 4)
for(i in seq_along(fact)) lines(Dim2 ~ Dim1, data = coords, subset = factor==fact[i], lwd = lwd[i], col = cols[i])
legend("bottomright", legend = c("Gender", "PreSex", "ExtraSex", "Marital"), title = "Factor", title.col = "black", col = cols, text.col = cols, pch = 16:19, bg = "gray95", cex = 1.2)



# -->
# The category points for each factor appear on lines through the origin, with distances inversely proportional to their marginal frequencies.
# The categories for No premarital and extramarital sex are much larger than the corresponding Yes categories.
# The categories of gender and marital status are more nearly equal marginally
# Dimension 1:  women are less likely to have had pre-marital and extramarital sex and still being married is associated with the absence of pre- and extra-marital sex.

# The lines for gender and marital status are nearly at right angles, suggesting that these variable are unassociated.
# This interpretation is more or less correct, but it is only approximate in this MCA scaling of the coordinate axes.

# If you compare the MCA result with mosaic matrix, you will see that they are both showing the bivariate pairwise associations among these variables,
# but in different ways.
# The mosaic plots show the details of marginal and joint frequencies together with residuals from independence for each 2 * 2 marginal subtable.
# The MCA plot using the Burt matrix summarized each category poin in terms of a 2D representation of contributions to total inertia (association).


pairs(PreSex, gp = shading_Friendly2, space = 0.25, gp_args = list(interpolate = 1:4), diag_panel_args = list(offset_varnames = -0.5))



