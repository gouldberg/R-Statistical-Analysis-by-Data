setwd("//media//kswada//MyFiles//R//squirrels")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Red Squirrels
#   - The red squirrels (Sciurus vulgaris L.) is an endangered species in Scotland and is therefore included in the UK Biodiversity Action Plan.
#     Long-term plans are in place to guarantee the future presence of this species in the UK.
#     To accomplish this it is important to know the effect of tree species composition and food availability on red squirrel abundance.
#   - Flaherty et al. (2012) studied the relationship between red squirrel habitat use and forest structure factors such as canopy connectivity,
#     tree density, and tree height heterogeneity.
#     Fieldwork was carried out in two forests in Scotland, Abernethy Forest and Aberfoyle.
#     Plots of 14 * 14 m were delineated:  20 in Aberfoyle and 32 in Abernethy.
#   - Within each plot, Flaterty et al. (2012) recorded tree height, diameter of trees at breast height (DBH), and canopy closure.
#     Canopy closure was the mean canopy closure measured with a spherical densitometer at the central point and at each of the four corners of the plot.
#     Diameter at breast height was recorded for all trees in the plot. Height of all trees was measured, and in plots with more than 30 trees,
#     the height of the four trees with the largest DBH was measured and recorded.
#   - Feeding remains of stripped cones were used as an index of microhabitat selection by red squirrels. All sampled plots contained coniferous species.
#     For further details on the sampling protocal we refer the reader to Flaherty et al. (2012)
#   - Variables:
#        - SqCones:  Number of cones stripped by squirrels
#        - Ntrees:  Number of trees per plot
#        - DBH:  Mean DBH per plot
#        - TreeHeight:  Mean tree height per plot
#        - CanopyCover:  Canopy closure (as a percentage)
#        - Id:  Site label
# ------------------------------------------------------------------------------

SQ <- read.table(file = "RedSquirrels.txt", header = TRUE, dec = ".")


str(SQ)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
