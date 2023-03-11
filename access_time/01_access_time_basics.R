# setwd("//media//kswada//MyFiles//R//access_time//")
# setwd("//media//kswada//MyFiles//R//Bayesian_inference//access_time//")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\access_time")

packages <- c("dplyr", "rstan")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  access time
# ------------------------------------------------------------------------------

data <- read.csv("access_time.csv", header = T)


str(data)




# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

