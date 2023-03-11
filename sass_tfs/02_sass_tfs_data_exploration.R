# setwd("//media//kswada//MyFiles//R//lalonde_nswcps")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\sass_tfs")

packages <- c("dplyr", "tidyverse", "MatchIt", "Matching", "survey")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  SASS and TFS
# ------------------------------------------------------------------------------


load("SASS_TFS_data_imputed.Rdata")

str(imputedData)

names(imputedData)




# ------------------------------------------------------------------------------
# data exploration
# ------------------------------------------------------------------------------


# the outcome

table(imputedData$Treat, useNA = "always")


