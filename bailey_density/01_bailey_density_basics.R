# setwd("//media//kswada//MyFiles//R//bailey_density")
# setwd("//media//kswada//MyFiles//R//Generalized_additive_model//bailey_density")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\bailey_density")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Bailey Density
#   - Presented in Bailey et al. (2008), who looked at the impact of commercial fishing on deep-sea fish populations/
#     They analysed data of a bottom trawl survey conducted in the northeast Atlantic Ocean from 1979 to 1989 and from 1997 to 2002.
#     Sampling took place at depths of 800 to 4865 m. All captured fish were retained and identified to species.
#   - Due to technical and financial considerations, commercial fishing is typically limited to depths of approximately 1600 m.
#     The first time period (1979 - 1989) can be considered as pre-commercial fishing, or contemporary with early development of the 
#     deep-water fishery, and the second period (1997 - 2002) as commerical fishing.
#     A wide variety of fish species inhabit this depth range, with each species having its own depth niche, which are partially overlapping.
#   - The underlying biological question is whether the stress imposed by commercial fishing on a portion of this depth gradient has
#     affected species assemblages throughout the entire depth gradient.
# ------------------------------------------------------------------------------

DF1 <- read.table(file = "BaileyDensity.txt", header = TRUE)


str(DF1)


dim(DF1)


car::some(DF1)



# ------------------------------------------------------------------------------
# Basics
# ------------------------------------------------------------------------------
