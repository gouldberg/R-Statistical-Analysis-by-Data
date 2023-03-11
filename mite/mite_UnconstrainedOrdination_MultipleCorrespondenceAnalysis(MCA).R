# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
rm(list=ls())
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr", "vegan", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  mite
#  - Substrate (7 classes),  Shrubs (3 classes), and Microtopography (2 classes)
# ------------------------------------------------------------------------------
# Load the oribatid mite data
load("./RefData/NumericalEcologyWithR/NEwR2-Data/mite.RData")
car::some(mite)
car::some(mite.env)
car::some(mite.xy)



# ------------------------------------------------------------------------------
# Multiple correspondence analysis (MCA)
#  - MCA is a special form of correspondence analysis where the variables are categorical.
#  - It has been designed primarily to analyse a series of individuals (e.g. persons in a survey, specimens in a taxonomic study) characterized by qualitative variables.
#  - function MCA() offers the possibility of projecting supplementary variables into the MCA result. Note that these supplementary variables are not involved
#   in the computation of the MCA itself, they are added to the result for heuristic prurposses only.
#
# ------------------------------------------------------------------------------
# Preparation of supplementary data: mite classification into 4 groups
# Hellinger transformation of oribatid mite species data
mite.h <- decostand(mite, "hel")


# Ward clustering of mite data
mite.h.ward <- hclust(dist(mite.h), "ward.D2")


# Cut the dendrogram to 4 groups
mite.h.w.g <- cutree(mite.h.ward, 4)


# Assembly of the data set:  mite.env + clustering of mite
mite.envplus <- data.frame(mite.env, mite.h.w.g)



# ---------------------------------------------------
# MCA of qualitative environmental data plus supplementary variables
#   (1) quantitative environmental data, 
#   (2) 4-group mite classification.
#   Default: graph=TRUE.
#
# 2 axes together represent 32.33% of the inertia of the dataset.
# (a) sites
# (b) levels (modalities) of the variables involved in the analysis and of the qualitative supplementary variable (groups 1 to 4 marked by green triangles)
# (c) R2 of the involved and supplementary variables with axes 1 and 2
# (d) projection of the supplementary quantitative variables
#
# Site 11 is isolated from all others at the top of the graph, the only one that contains Sphagnum group 3
# Pairs of sites 44 + 57 (left) and 21 + 26 (right) stand also relatively apart.
# The 44 + 57 pair shares the "Barepeat" modality and the pair 21 + 26 shares the "Sphagn4" modality. These 2 modalities have only 2 occurrences in the data set.
# These results show that MCA shares with CA the property of emphasizing rare events, which may be useful to identify rare characteristics or otherwise special features in a qualitative dataset.
# The sites that are closest to the forest (smal numbers) are on the right, and the sites that are clsest tot the free weter (large numbers) are on the left.
#
# (c) represents the squared correlation of the variables with the ordination axes. This graph can be used to identify the variables that are most related to the axes.
# Variable "Topo" (blanket vs hummock) has an R^2 = 0.7433 on axis 1 but only 0.0262 on axis 2 (mite.env.MCA$var$eta2)

graphics.off();  par(mfrow=c(2,2))
mite.env.MCA <- FactoMineR::MCA(mite.envplus, quanti.sup = 1:2, quali.sup = 6)


# 2 axes together represent 32.33% of the inertia of the dataset.
mite.env.MCA$eig


# (c) the squared correlation of the variables with ordination axes.
mite.env.MCA$var$eta2


# The contributions of the modalities to the axes.
# On axis 1, the highest contributions are those of modalities Hummock, Shurub-None and Blanket
# On axis 2, Shurubs-Many, Sphagn3 and Srubs-Few stand out. This points to the great importance of the 2 variables "Topo", and "Shrub".
mite.env.MCA$var$contrib


# Contingency table crossing variables "Shrub" and "Topo"
# The table shows that sites where shrubs are present (modalities "Few" and "Many") are rather evenly represented on blankets and Hummocks, 
# but all sites devoid of shrubs have blanket-type soil coverage.
table(mite.env$Shrub, mite.env$Topo)


