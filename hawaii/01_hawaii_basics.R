# setwd("//media//kswada//MyFiles//R//hawaii")
# setwd("//media//kswada//MyFiles//R//Generalized_linear_model_and_heterogeneity//hawaii")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\hawaii")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Hawaii
#   - Read et al. (2007) analysed abundances of three bird species measured at 3 islands in Hawaii.
#     The data were annual abundances from 1956 to 2003.
# ------------------------------------------------------------------------------

Hawaii <- read.table(file = "Hawaii.txt", header = TRUE)


str(Hawaii)


dim(Hawaii)


car::some(Hawaii)




# ------------------------------------------------------------------------------
# Basics
# ------------------------------------------------------------------------------

