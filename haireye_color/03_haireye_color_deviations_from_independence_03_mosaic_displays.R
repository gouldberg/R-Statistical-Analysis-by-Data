setwd("//media//kswada//MyFiles//R//haireye_color")

packages <- c("dplyr", "datasets", "vcd", "vcdExtra", "MASS")
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
# mosaic display
#  - is similar to the sieve diagram. However, mosaic plots and related methods
#     - generalize more readily to n-way tables. One can usefully examine 3-way, 4-way, and even larger tables, subject to the limitations of resolution in any graph.
#     - are intimately connected to loglinear models, generalized linear models, and generalized non-linear models for frequency data
#     - provide a method for fitting a series of sequential loglinear models to the various marginal totals of an n-way table
#     - can be used to illustrate the relations among variables that are fitted by various loglinear models
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# mosaic display:  Two-way tables
# without shading (simple one)
#
#  - The mosaic dsiplay is like a grouped barchard, where the heights (or widths) of the bars show the relative frequencies of one variable,
#    and widths (heights) of the sections in each bar show the conditional frequencies of second variable, given the first.
#  - This gives an area-proportional visualization of the frequencies composed of tiles correspoinding to the cells created
#    by successive vertical and horizontal splits of rectangle, representing the total frequency in the table.
# ------------------------------------------------------------------------------
mosaic(haireye, labeling = labeling_values)



# ----------
# calculated marginal frequencies and proportions of hair color
( hair <- margin.table(haireye, 1) )

prop.table(hair)



# ----------
vcd::mosaic(haireye, pop = FALSE)
vcd::labeling_cells(text = haireye, gp_text = gpar(fontface = 2), clip = FALSE)(haireye)



# ------------------------------------------------------------------------------
# Two-way tables
# Mosaic plot with shading and stantardized Pearson residuals
#  - The simplest (and default) shading patterns for the tiles are based on the sign and magnitude of the standardized Pearson residuals,
#    using shades of blue for positive residuals adn red for negative residuals, and two threshold values for their magnitudes, |r(ij)| > 2 and |r(ij)| > 4
#  - Because the standardized residuals are approximately unit-normal N(0,1) values, this corresponds to highlighting cells whose residuals are individually significant at approximately the 0.05 and 0.0001 level, respectively.
# ------------------------------------------------------------------------------
# ----------
# rectangle in mosaic plot
expected <- rep(sum(hair) / 4, 4)
names(expected) <- names(hair)
expected


( residuals <- (hair - expected) / sqrt(expected) )


# the rectangle for each hair color is subdivided in proportion to the relative (conditinoal) frequencies of the second variable, eye color,
# giving the following conditional row proportions
round(addmargins(prop.table(haireye, 1), 2), 3)



# ----------
# standardized Pearson residuals
# The cells are shaded in relation to standardized Pearson residuals from a model.
# For a two-way table, the model is that hair color and eye color are independent in the population from which this sample was drawn.
# These residuals are calculated using independence_table() to caluculate the expected values under this model
exp <- independence_table(haireye)
exp

resids <- (haireye - exp) / sqrt(exp)
round(resids, 2)

round(residuals(chisq.test(haireye)), 2)



# ----------
# Each rectangle for hair color is subdivided in proportaion to the relative frequencies of eye color
# and the tile are shaded in relation to residuals from the model of independence
# The tile shaded deep red (Blond, Brown), corresponds to the largest negative residual = -5.85,
# indicating this combination is extremely rare under the hypothesis of independence.
vcd::mosaic(haireye, shade=TRUE, suppress=0, labeling=labeling_residuals, gp_text=gpar(fontface=2))

vcd::mosaic(haireye, shade = TRUE, labeling = labeling_residuals)



# ----------
# Overall Pearson chi-square statistic for the independence model
( chisq <- sum(resids ^ 2) )
( df <- prod(dim(haireye) - 1) )
pchisq(chisq, df, lower.tail = FALSE)

chisq.test(haireye)



# ------------------------------------------------------------------------------
# Two-way tables
# Reordering and interpretaion
# ------------------------------------------------------------------------------
# The interpretation can often be enhanced by reordering the rows and columns of the 2-way table
# so that the residuals have an opposite corner pattern of signs.
# In general, the levels of a factor in mosaic displays are often best reordered by arranging them according to their scores on the first (largest) correspondence analysis dimension.
# re-order eye colors from dark to light
haireye2 <- as.table(haireye[, c("Brown", "Hazel", "Green", "Blue")])
vcd::mosaic(haireye2, shade = TRUE)


# --> 
# people with dark hair tend to have dark eyes,
# those with light hair tend to have light eyes,
# people with red hair and green eyes do not quite fit this pattern
