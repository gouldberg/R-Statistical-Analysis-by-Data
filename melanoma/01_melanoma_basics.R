# setwd("//media//kswada//MyFiles//R//melanoma")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\melanoma")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  melanoma
#   - Contains age-adjusted incidences of melanoma from the Connecticut Tumor Registry for the years 1936 to 1972.
#   - The data are numbers of melanoma cases per 100,000 in the state of Connecticut for the years 1936 to 1972.
#     The data show two kinds of trend:
#        - a nearly linear long-term trend over this period,
#          and a cycle of around 10 years corresponding to the sunspot cycle.  
# ------------------------------------------------------------------------------

melanoma <- t(matrix(scan("melanoma.txt", 0), 3, 37)) %>% as.data.frame() %>% dplyr::select(V2, V3)

# data("melanoma", package = "fda")


colnames(melanoma) <- c("year", "incidence")



str(melanoma)




# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
