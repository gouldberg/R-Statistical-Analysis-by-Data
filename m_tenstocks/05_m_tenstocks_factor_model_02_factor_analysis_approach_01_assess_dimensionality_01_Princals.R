# setwd("//media//kswada//MyFiles//R//email")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\m_tenstocks")


packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  m-tenstocks
# ------------------------------------------------------------------------------

mtenstocks <- read.csv("m-tenstocks.txt", sep = " ", header = T)


str(mtenstocks)


dim(mtenstocks)


car::some(mtenstocks)



# ----------
# log returns

rtn <- log(mtenstocks[,2:11] + 1)



# ------------------------------------------------------------------------------
# standardize individual series
# ------------------------------------------------------------------------------


std <- diag(1 / sqrt(diag(cov(rtn))))


rtns <- as.matrix(rtn) %*% std




# ------------------------------------------------------------------------------
# Assess unidimensionality
#   - fitting a 2-dimensional Princals in order to get a picture of item associations in a 2D space.
# ------------------------------------------------------------------------------

library(Gifi)


prin <- princals(rtn)
# prin <- princals(rtns)


prin



# ----------
# plot loadings
plot(prin, main = "log returns Loadings")



# -->
# almost 3 components


