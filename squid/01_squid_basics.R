# setwd("//media//kswada//MyFiles//R//squid")
# setwd("//media//kswada//MyFiles//R//Generalized_linear_model_and_heterogeneity//squid")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\squid")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Squid
#   - Data published by Smith et al. (2005), who looked at seasonal patterns in reproductive and somatic tissues in the squid Loligo forbesi.
#   - They used several variables on female and male squid, but we only use the dorsal mantle length (in mm) and testis weight from 768 male squid.
#     The aim is to model the testis weight as a function of the dorsal mantle length (DML) and the month recorded.
#     The idea behind the original analysis was to investigate the role of endogenous and exogenous factors affecting sexual maturation,
#     more specifically to determine the extent to which maturation is size-related and seasonal.
# ------------------------------------------------------------------------------

Squid <- read.table(file = "Squid.txt", header = TRUE)


str(Squid)


dim(Squid)


car::some(Squid)



# ------------------------------------------------------------------------------
# Basics
# ------------------------------------------------------------------------------
